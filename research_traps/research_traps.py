import subprocess
import json
import chess
import os
import re

# =============================================================================
# CONFIGURATION
# =============================================================================
# Paths
CHESS_QUERY_EXE = r"D:\Github\Grampus\x64\Release\ChessQuery.exe"
STOCKFISH_EXE = r"D:\Github\mcp-stockfish-master\stockfish.exe"
OUTPUT_FILE = r"D:\Github\Grampus\research_traps\Nf3_black_surprising_moves.txt"

# Search Parameters
HUNT_SIDE = chess.BLACK   # Options: chess.WHITE or chess.BLACK
START_PATH = "1. Nf3"  # Format: "1. e4 e6 2. d4" etc.

# Thresholds
MIN_GAMES_FOR_MOVE = 30      # Minimum games to trust the statistical data
WIN_RATE_THRESHOLD = 0.35    # Success rate must be at least this (0.0 - 1.0)
WIN_RATE_OUTLIER_GAP = 0.10  # Move must be this much better than the average of others
MAX_PLY = 30                 # Search depth in half-moves
# =============================================================================

def get_fen_from_path(path_str):
    """
    Parses a string like '1. e4 e6 2. d4' and returns the resulting FEN.
    """
    board = chess.Board()
    # Remove move numbers (e.g., '1.', '2...') and split into individual moves
    # This regex removes things like "1." or "1..."
    clean_path = re.sub(r'\d+\.(\.\.)?', '', path_str)
    moves = clean_path.split()
    
    for move_san in moves:
        try:
            board.push_san(move_san)
        except ValueError:
            print(f"Error: Could not parse move '{move_san}' in START_PATH.")
            return None
    return board.fen()

def get_db_stats(fen):
    """Query the C++ LMDB database via the bridge tool."""
    res = subprocess.run([CHESS_QUERY_EXE, fen], capture_output=True, text=True)
    try:
        return json.loads(res.stdout)
    except:
        return None

def get_engine_eval(fen, perspective):
    """Returns score relative to the side we are hunting."""
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
    
    eval_val = score / 100.0
    return eval_val if perspective == chess.WHITE else -eval_val

def find_surprising_move(stats):
    """Finds if a move is a statistical outlier."""
    moves = stats['moves']
    valid_moves = []
    
    for m in moves:
        total = m['w'] + m['d'] + m['l']
        if total >= MIN_GAMES_FOR_MOVE:
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

    if board.turn == HUNT_SIDE:
        surprising = find_surprising_move(stats)
        if surprising:
            test_board = board.copy()
            test_board.push_uci(surprising['uci'])
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

    moves = sorted(stats['moves'], key=lambda x: x['w'] + x['d'] + x['l'], reverse=True)
    for m_data in moves[:2]:
        total = m_data['w'] + m_data['d'] + m_data['l']
        if total < MIN_GAMES_FOR_MOVE:
            continue
            
        new_board = board.copy()
        try:
            move_obj = chess.Move.from_uci(m_data['uci'])
            san = board.san(move_obj)
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
        
    start_fen = get_fen_from_path(START_PATH)
    
    if start_fen:
        side_str = "WHITE" if HUNT_SIDE == chess.WHITE else "BLACK"
        print(f"Scanning for surprising {side_str} moves...")
        print(f"Start Path: {START_PATH}")
        print(f"Calculated FEN: {start_fen}")
        
        # Determine the initial depth based on moves pushed
        initial_board = chess.Board(start_fen)
        initial_depth = len(initial_board.move_stack) # Not accurate for FEN, so we use fullmove*2
        # Let's use a simpler depth based on move number
        initial_depth = (initial_board.fullmove_number - 1) * 2 + (0 if initial_board.turn == chess.WHITE else 1)

        crawl(start_fen, START_PATH, initial_depth)
        print(f"\nSearch complete. Results saved to {OUTPUT_FILE}")