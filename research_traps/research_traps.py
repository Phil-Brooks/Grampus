import subprocess
import json
import chess
import os

# --- CONFIGURATION ---
CHESS_QUERY_EXE = r"D:\Github\Grampus\x64\Release\ChessQuery.exe"
STOCKFISH_EXE = r"D:\Github\mcp-stockfish-master\stockfish.exe"
OUTPUT_FILE = r"D:\Github\Grampus\research_traps\surprising_black_moves.txt"

# Thresholds
MIN_GAMES_FOR_MOVE = 30      # Minimum games to trust the statistical data
WIN_RATE_THRESHOLD = 0.35    # Black must win at least 40% of the time (l/total)
WIN_RATE_OUTLIER_GAP = 0.10  # Move must be at least 12% better than other common moves
MAX_PLY = 30                 # Search up to Move 15

def get_db_stats(fen):
    res = subprocess.run([CHESS_QUERY_EXE, fen], capture_output=True, text=True)
    try:
        return json.loads(res.stdout)
    except:
        return None

def get_engine_eval_for_black(fen):
    """Returns score from Black's perspective (+ means Black is better)"""
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
    return (score / 100.0) * -1

def find_surprising_black_move(stats):
    """Finds a Black move that is an outlier with > 40% win rate"""
    moves = stats['moves']
    valid_moves = []
    
    for m in moves:
        total = m['w'] + m['d'] + m['l']
        if total >= MIN_GAMES_FOR_MOVE:
            # m['w'] is the perspective win (Black's win if it's Black's turn)
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

    # Sort by Black's win rate
    valid_moves.sort(key=lambda x: x['win_rate'], reverse=True)
    
    best = valid_moves[0]
    others = valid_moves[1:]
    avg_others = sum(m['win_rate'] for m in others) / len(others)

    # Highlight if the move is significantly better than the alternatives
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
    is_black_turn = (board.turn == chess.BLACK)
    move_number = board.fullmove_number

    if is_black_turn:
        surprising = find_surprising_black_move(stats)
        if surprising:
            test_board = board.copy()
            test_board.push_uci(surprising['uci'])
            black_score = get_engine_eval_for_black(test_board.fen())

            report = (
                f"SURPRISING MOVE FOR BLACK (Move {move_number})\n"
                f"Line: {path_string}\n"
                f"FEN Before: {fen}\n"
                f"Surprising Move: {board.san(chess.Move.from_uci(surprising['uci']))} ({surprising['uci']})\n"
                f"Black Win Rate: {surprising['win_rate']:.1%} (Total Games: {surprising['total']})\n"
                f"Stockfish Evaluation (Black Relative): {black_score:+.2f}\n"
                f"Stats: Wins:{surprising['stats']['w']} Draws:{surprising['stats']['d']} Losses:{surprising['stats']['l']}\n"
                f"{'='*60}\n"
            )
            print(report)
            with open(OUTPUT_FILE, "a") as f:
                f.write(report)

    # Sort by popularity to find the "Main Lines"
    moves = sorted(stats['moves'], key=lambda x: x['w'] + x['d'] + x['l'], reverse=True)
    
    # Follow the top 2 variations to keep research focused on relevant lines
    for m_data in moves[:2]:
        total = m_data['w'] + m_data['d'] + m_data['l']
        if total < MIN_GAMES_FOR_MOVE:
            continue
            
        new_board = board.copy()
        try:
            move_obj = chess.Move.from_uci(m_data['uci'])
            san = board.san(move_obj)
            
            # Format path with PGN numbering
            if board.turn == chess.WHITE:
                new_path = f"{path_string} {move_number}. {san}"
            else:
                new_path = f"{path_string} {san}"
                
            new_board.push(move_obj)
            crawl(new_board.fen(), new_path, depth + 1)
        except:
            continue

if __name__ == "__main__":
    # Starting Position for French: after 1. e4 e6 (Black to move)
    start_fen = "rnbqkbnr/pppp1ppp/4p3/8/4P3/8/PPPP1PPP/RNBQKBNR w KQkq - 0 2"
    
    if os.path.exists(OUTPUT_FILE):
        os.remove(OUTPUT_FILE)
        
    print(f"Scanning for surprising Black moves in the French Defense...")
    print(f"Results will be saved to: {OUTPUT_FILE}")
    
    # Initial path reflects White's first move
    crawl(start_fen, "1. e4 e6", 1)
    
    print(f"\nSearch complete. Check {OUTPUT_FILE} for discoveries.")