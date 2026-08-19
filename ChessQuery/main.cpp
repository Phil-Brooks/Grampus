#include <iostream>
#include <vector>
#include <string>
#include "chess.hpp"
#include "lmdb.h"

using namespace chess;

#pragma pack(push, 1)
struct MoveEntry {
    uint16_t move_id = 0;
    uint8_t ply = 0;
    uint32_t wins = 0;
    uint32_t draws = 0;
    uint32_t losses = 0;
};
#pragma pack(pop)

int main(int argc, char* argv[]) {
    if (argc < 2) {
        std::cerr << "Error: No FEN provided." << std::endl;
        return 1;
    }

    std::string fen = argv[1];
    Board board;

    try {
        board.setFen(fen);
    }
    catch (...) {
        std::cerr << "Error: Invalid FEN string." << std::endl;
        return 1;
    }

    uint64_t hash = board.zobrist();

    MDB_env* env;
    mdb_env_create(&env);

    // REMOVED MDB_NOSUBDIR because your DB is in a folder
    int rc = mdb_env_open(env, "D:/pgns/all15_db", MDB_RDONLY, 0664);
    if (rc != 0) {
        std::cerr << "LMDB Error (env_open): " << mdb_strerror(rc) << std::endl;
        std::cerr << "Check if D:/pgns/all15_db exists and contains data.mdb" << std::endl;
        return 1;
    }

    MDB_txn* txn;
    MDB_dbi dbi;
    mdb_txn_begin(env, NULL, MDB_RDONLY, &txn);
    mdb_dbi_open(txn, NULL, 0, &dbi);

    MDB_val key, data;
    key.mv_size = sizeof(uint64_t);
    key.mv_data = &hash;

    if (mdb_get(txn, dbi, &key, &data) == 0) {
        MoveEntry* entries = (MoveEntry*)data.mv_data;
        size_t count = data.mv_size / sizeof(MoveEntry);

        std::cout << "{\"found\": true, \"ply\": " << (int)entries[0].ply << ", \"moves\": [";
        for (size_t i = 0; i < count; ++i) {
            Move m = Move(entries[i].move_id);
            std::cout << "{"
                << "\"uci\": \"" << uci::moveToUci(m) << "\","
                << "\"w\": " << entries[i].wins << ","
                << "\"d\": " << entries[i].draws << ","
                << "\"l\": " << entries[i].losses << "}"
                << (i == count - 1 ? "" : ",");
        }
        std::cout << "]}" << std::endl;
    }
    else {
        // If the position isn't in the DB, we should still see this:
        std::cout << "{\"found\": false}" << std::endl;
    }

    mdb_txn_abort(txn);
    mdb_env_close(env);
    return 0;
}