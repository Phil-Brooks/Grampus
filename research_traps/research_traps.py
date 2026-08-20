import subprocess
import json
import chess
import os

# =============================================================================
# CONFIGURATION
# =============================================================================
# Paths
CHESS_QUERY_EXE = r"D:\Github\Grampus\x64\Release\ChessQuery.exe"
STOCKFISH_EXE = r"D:\Github\mcp-stockfish-master\stockfish.exe"
OUTPUT_FILE = r"D:\Github\Grampus\research_traps\Nf3_black_surprising_moves.txt"

# Search Parameters
HUNT_SIDE = chess.BLACK  # Options: chess.WHITE or chess.BLACK
START_FEN = "rnbqkbnr/pppppppp/8/8/8/5N2/PPPPPPPP/RNBQKB1R b KQkq - 1 1"
START_PATH = "1. Nf3"

# Thresholds
MIN_GAMES_FOR_MOVE = 30      # Minimum games to trust the statistical data
WIN_RATE_THRESHOLD = 0.35    # Success rate must be at least this (0.0 - 1.0)
WIN_RATE_OUTLIER_GAP = 0.10  # Move must be this much better than the average of others
MAX_PLY = 30                 # Search depth in half-moves
# =============================================================================

def get_db_stats(fen):
    """Query the C++ LMDB database via the bridge tool."""
    res = subprocess.run([CHESS_QUERY_EXE, fen], capture_output=True, text=True)
    try:
        return json.loads(res.stdout)
    except:
        return None

def get_engine_eval(fen, perspective):
    """
    Returns score relative to the side we are hunting.
    If hunting White: Positive is good for White.
    If hunting Black: Positive is good for Black.
    """
    engine = subprocess.Popen(STOCKFISH_EXE, stdin=subprocess.PIPE, stdout=subprocess.PIPE, text=True)
    engine.stdin.write(f"position fen {fen}\ngo depth 15\n")
    engine.stdin.flush()
    
    score = 0
    while True:
        line = engine.stdout.readline()
        if "score cp" in line:
            parts = line.split()
            score = int(parts[parts.index("cp") + 1])
        if "bestmove" in line:
            break
    engine.terminate()
    
    # Stockfish cp is always relative to White.
    # If hunting Black, flip the sign.
    eval_val = score / 100.0
    return eval_val if perspective == chess.WHITE else -eval_val

def find_surprising_move(stats):
    """
    Finds if a move is a statistical outlier.
    Because the DB was built with 'perspective wins', m['w'] is always 
    the win rate for the player whose turn it is.
    """
    moves = stats['moves']
    valid_moves = []
    
    for m in moves:
        total = m['w'] + m['d'] + m['l']
        if total >= MIN_GAMES_FOR_MOVE:
            # win_rate for the side-to-move
            win_rate = m['w'] / total
            if win_rate >= WIN_RATE_THRESHOLD:
                valid_moves.append({
                    'uci': m['uci'], 
                    'win_rate': win_rate, 
                    'total': total, 
                    'stats': m
                })

    if len(valid_moves) < 2:
        return None

    # Sort by success rate
    valid_moves.sort(key=lambda x: x['win_rate'], reverse=True)
    
    best = valid_moves[0]
    others = valid_moves[1:]
    avg_others = sum(m['win_rate'] for m in others) / len(others)

    if best['win_rate'] > (avg_others + WIN_RATE_OUTLIER_GAP):
        return best
    
    return None

visited_fens = set()

def crawl(fen, path_string, depth):
    if depth > MAX_PLY or fen in visited_fens:
        return
    visited_fens.add(fen)

    stats = get_db_stats(fen)
    if not stats or not stats.get("found"):
        return

    board = chess.Board(fen)
    move_number = board.fullmove_number
    side_label = "WHITE" if HUNT_SIDE == chess.WHITE else "BLACK"

    # Check if the current side to move is the side we are hunting
    if board.turn == HUNT_SIDE:
        surprising = find_surprising_move(stats)
        if surprising:
            test_board = board.copy()
            test_board.push_uci(surprising['uci'])
            # Get eval from the perspective of the side we are hunting
            relative_score = get_engine_eval(test_board.fen(), HUNT_SIDE)

            report = (
                f"SURPRISING MOVE FOR {side_label} (Move {move_number})\n"
                f"Line: {path_string}\n"
                f"FEN Before: {fen}\n"
                f"Surprising Move: {board.san(chess.Move.from_uci(surprising['uci']))} ({surprising['uci']})\n"
                f"Win Rate: {surprising['win_rate']:.1%} (Total Games: {surprising['total']})\n"
                f"Stockfish Evaluation ({side_label} Relative): {relative_score:+.2f}\n"
                f"Stats: Wins:{surprising['stats']['w']} Draws:{surprising['stats']['d']} Losses:{surprising['stats']['l']}\n"
                f"{'='*60}\n"
            )
            print(report)
            with open(OUTPUT_FILE, "a") as f:
                f.write(report)

    # Recursive Tree Walk
    moves = sorted(stats['moves'], key=lambda x: x['w'] + x['d'] + x['l'], reverse=True)
    
    # Follow top 2 variations to explore the main tree
    for m_data in moves[:2]:
        total = m_data['w'] + m_data['d'] + m_data['l']
        if total < MIN_GAMES_FOR_MOVE:
            continue
            
        new_board = board.copy()
        try:
            move_obj = chess.Move.from_uci(m_data['uci'])
            san = board.san(move_obj)
            
            # Formatting the PGN path string
            if board.turn == chess.WHITE:
                new_path = f"{path_string} {move_number}. {san}"
            else:
                new_path = f"{path_string} {san}"
                
            new_board.push(move_obj)
            crawl(new_board.fen(), new_path, depth + 1)
        except:
            continue

if __name__ == "__main__":
    if os.path.exists(OUTPUT_FILE):
        os.remove(OUTPUT_FILE)
        
    side_str = "WHITE" if HUNT_SIDE == chess.WHITE else "BLACK"
    print(f"Scanning for surprising {side_str} moves...")
    print(f"Start: {START_PATH}")
    print(f"Results: {OUTPUT_FILE}")
    
    # Start the recursive crawl
    crawl(START_FEN, START_PATH, 1)
    
    print(f"\nSearch complete. Results saved to {OUTPUT_FILE}")