#include <iostream>
#include <fstream>
#include <vector>
#include <string>
#include <string_view>
#include <unordered_map>
#include <filesystem>
#include <memory> // Added for unique_ptr
#include "chess.hpp"
#include "lmdb.h"

using namespace chess;

// 1. Fixed uninitialized variable warning
#pragma pack(push, 1)
struct MoveEntry {
    uint16_t move_id = 0;
    uint8_t ply = 0;      // Half-move count (0-30)
    uint32_t wins = 0;
    uint32_t draws = 0;
    uint32_t losses = 0;
};
#pragma pack(pop)

typedef std::unordered_map<uint64_t, std::unordered_map<uint16_t, MoveEntry>> StatsCache;

class ChessDBVisitor : public pgn::Visitor {
public:
    StatsCache& cache;
    Board board;
    int current_result = 0;
    int ply_count = 0; // Track half-moves

    ChessDBVisitor(StatsCache& c) : cache(c) {}

    void startPgn() override {
        board.setFen(constants::STARTPOS);
        current_result = 0;
        ply_count = 0;
    }

    void header(std::string_view key, std::string_view value) override {
        if (key == "Result") {
            if (value == "1-0") current_result = 1;
            else if (value == "0-1") current_result = -1;
            else current_result = 0;
        }
    }

    void startMoves() override {}

    void move(std::string_view move_san, std::string_view comment) override {
        // STOP after 30 half-moves (15 full moves)
        if (ply_count >= 30) return;

        uint64_t hash = board.zobrist();
        Move m = uci::parseSan(board, move_san);
        if (m == Move::NO_MOVE) return;

        uint16_t move_id = m.move();

        // Get or create entry for this move AT THIS PLY
        auto& entry = cache[hash][move_id];
        entry.move_id = move_id;
        entry.ply = (uint8_t)ply_count; // Store the depth

        bool white_to_move = (board.sideToMove() == Color::WHITE);
        if (current_result == 0) entry.draws++;
        else if ((current_result == 1 && white_to_move) || (current_result == -1 && !white_to_move))
            entry.wins++;
        else
            entry.losses++;

        board.makeMove(m);
        ply_count++;
    }

    void endPgn() override {}
};

void flush_cache_to_db(MDB_env* env, MDB_dbi dbi, StatsCache& cache) {
    MDB_txn* txn;
    if (mdb_txn_begin(env, NULL, 0, &txn) != 0) return;

    for (auto& [hash, moves] : cache) {
        MDB_val key, data;
        key.mv_size = sizeof(uint64_t);
        key.mv_data = (void*)&hash;

        std::vector<MoveEntry> disk_entries;

        if (mdb_get(txn, dbi, &key, &data) == 0) {
            MoveEntry* existing = (MoveEntry*)data.mv_data;
            size_t count = data.mv_size / sizeof(MoveEntry); // Fixed size_t warning
            disk_entries.assign(existing, existing + count);
        }

        for (auto& [move_id, ram_entry] : moves) {
            bool found = false;
            for (auto& de : disk_entries) {
                if (de.move_id == move_id) {
                    de.wins += ram_entry.wins;
                    de.draws += ram_entry.draws;
                    de.losses += ram_entry.losses;
                    found = true;
                    break;
                }
            }
            if (!found) disk_entries.push_back(ram_entry);
        }

        data.mv_size = disk_entries.size() * sizeof(MoveEntry);
        data.mv_data = disk_entries.data();
        mdb_put(txn, dbi, &key, &data, 0);
    }

    mdb_txn_commit(txn);
    cache.clear();
}

int main() {
    // DB Setup
    MDB_env* env;
    mdb_env_create(&env);
    mdb_env_set_mapsize(env, 20ULL * 1024 * 1024 * 1024);

    std::filesystem::create_directories("d:/pgns/chess_db");
    if (mdb_env_open(env, "d:/pgns/chess_db", 0, 0664) != 0) {
        std::cerr << "Failed to open LMDB." << std::endl;
        return 1;
    }

    MDB_txn* txn;
    MDB_dbi dbi;
    mdb_txn_begin(env, NULL, 0, &txn);
    mdb_dbi_open(txn, NULL, MDB_CREATE, &dbi);
    mdb_txn_commit(txn);

    std::ifstream file("d:/pgns/french_early.pgn");
    if (!file.is_open()) return 1;

    // Use Heap allocation to fix C6262 stack warning
    auto cache = std::make_unique<StatsCache>();
    auto visitor = std::make_unique<ChessDBVisitor>(*cache);

    pgn::StreamParser parser(file);

    std::cout << "Starting build. This may take a while..." << std::endl;

    // We can't easily count games without a loop, so let's just run.
    parser.readGames(*visitor);

    // Final flush
    flush_cache_to_db(env, dbi, *cache);

    std::cout << "Build Finished!" << std::endl;
    mdb_env_close(env);
    return 0;
}