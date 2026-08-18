import subprocess
import json
import sys
import os

# PATH TO YOUR COMPILED QUERY TOOL
QUERY_TOOL_PATH = r"D:\Github\Grampus\x64\Release\ChessQuery.exe"

def get_human_stats(fen: str):
    """Bridge to the C++ database tool"""
    try:
        result = subprocess.run([QUERY_TOOL_PATH, fen], capture_output=True, text=True)
        # Return the raw string from C++, which is already JSON
        return result.stdout.strip()
    except Exception as e:
        return json.dumps({"error": str(e)})

def send_response(response):
    """Helper to send JSON-RPC response to stdout"""
    sys.stdout.write(json.dumps(response) + "\n")
    sys.stdout.flush()

def main():
    # Manual Test Mode
    if len(sys.argv) > 1:
        print(get_human_stats(sys.argv[1]))
        return

    for line in sys.stdin:
        if not line.strip():
            continue
        try:
            request = json.loads(line)
            method = request.get("method")
            req_id = request.get("id")

            # 1. Handle the Handshake (MANDATORY)
            if method == "initialize":
                response = {
                    "jsonrpc": "2.0",
                    "id": req_id,
                    "result": {
                        "protocolVersion": "2024-11-05",
                        "capabilities": {
                            "tools": {}
                        },
                        "serverInfo": {
                            "name": "french-defense-stats",
                            "version": "1.0.0"
                        }
                    }
                }
                send_response(response)

            # 2. Handle Tool Listing
            elif method == "tools/list":
                response = {
                    "jsonrpc": "2.0",
                    "id": req_id,
                    "result": {
                        "tools": [
                            {
                                "name": "get_human_stats",
                                "description": "Get human move stats for French Defense positions",
                                "inputSchema": {
                                    "type": "object",
                                    "properties": {
                                        "fen": {"type": "string", "description": "The FEN of the position"}
                                    },
                                    "required": ["fen"]
                                }
                            }
                        ]
                    }
                }
                send_response(response)

            # 3. Handle Tool Execution
            elif method == "tools/call":
                params = request.get("params", {})
                args = params.get("arguments", {})
                fen = args.get("fen")
                
                stats_json = get_human_stats(fen)
                
                response = {
                    "jsonrpc": "2.0",
                    "id": req_id,
                    "result": {
                        "content": [
                            {"type": "text", "text": stats_json}
                        ]
                    }
                }
                send_response(response)
            
            # 4. Handle Lifecycle Notifications
            elif method == "notifications/initialized":
                pass # Claude is ready

        except Exception as e:
            # Silently ignore or log to stderr for debugging
            sys.stderr.write(f"Error: {str(e)}\n")

if __name__ == "__main__":
    main()