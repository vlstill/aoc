#include <cassert>
#include <algorithm>
#include <numeric>
#include <iostream>
#include <iterator>
#include <map>
#include <set>
#include <sstream>
#include <string>
#include <string_view>
#include <vector>
#include <deque>
#include <optional>
#include <list>
#include <limits>
#include <queue>
#include <iomanip>
#include <memory>
#include <variant>
#include <type_traits>
#include <cmath>

int move_energy(char a) {
    return std::pow(10, a - 'A');
}

struct Antipodium {
    std::array<char, 11> hallway{};
    std::array<std::array<char, 4>, 4> burrows{};

    auto operator<=>(const Antipodium &) const = default;

    int dst_burrow_door(char a) {
	return 2 + 2 * (a - 'A');
    }

    int dst_burrow_idx(char a) {
	return a - 'A';
    }

    bool sorted() {
	for (auto v : hallway) {
	    if (v != 0)
		return false;
	}
	for (int i = 0; i < ssize(burrows); ++i) {
	    for (auto a : burrows[i])
		if (i != dst_burrow_idx(a))
		    return false;
	}
	return true;
    }

    std::optional<std::pair<Antipodium, long>> go_home(char who, int from_where) {
        int door = dst_burrow_door(who);
        int where = who - 'A';
        for (auto a : burrows[where])
            if (a != 0 && a != who)
                return std::nullopt;
        int dir = door > from_where ? 1 : -1;
        for (int i = from_where + dir; i != door + dir; i += dir) {
            if (hallway[i] != 0)
                return std::nullopt;
        }
        auto copy(*this);
        copy.hallway[from_where] = 0;
        for (int i : {3, 2, 1, 0}) {
            if (copy.burrows[where][i] == 0) {
                copy.burrows[where][i] = who;
                return std::pair(copy, (i + 1 + std::abs(from_where - door)) * move_energy(who));
            }
        }
        __builtin_trap();
    }

    void go_out(char who, int bur, int idx, auto yield) {
        for (int i = idx - 1; i >= 0; --i)
            if (burrows[bur][i] != 0)
                return;
        auto pos = 2 + 2 * bur;
        for (auto dir : {1, -1}) {
            for (int i = pos + dir; i >= 0 && i < ssize(hallway); i += dir) {
                if (i == 2 || i == 4 || i == 6 || i == 8)
                    continue;
                if (hallway[i] != 0)
                    break;
                auto copy(*this);
                copy.burrows[bur][idx] = 0;
                copy.hallway[i] = who;
                yield(copy, move_energy(who) * (idx + 1 + std::abs(i - pos)));
            }
        }
    }

    void dump(int depth) {
        std::string pad(depth * 4, ' ');
        std::cerr << pad << std::string(13, '#') << '\n';
        std::cerr << pad << '#';
        for (auto v : hallway)
            std::cerr << (v ?: '.');
        std::cerr << "#\n";
        std::cerr << pad << "###";
        auto b = [&](int i) {
            for (auto bur : burrows)
                std::cerr << (bur[i] ?: '.') << '#';
        };
        b(0);
        std::cerr << "##\n";
        for (int i : {1, 2, 3}) {
            std::cerr << pad << "  #";
            b(i);
            std::cerr << "\n";
        }
        std::cerr << pad << "  " << std::string(9, '#') << '\n';
    }
};

std::optional<long> solve(Antipodium a, long current_energy, long current_max, auto &seen, int depth = 0) {
    if (current_energy > current_max)
	return std::nullopt;

    if (auto it = seen.find(a); it != seen.end() && it->second <= current_energy)
        return std::nullopt;

    seen[a] = current_energy;

    if (a.sorted()) {
	return current_energy;
    }

    long local_best = current_max;
    int local_solved = 0;
    for (int i = 0; i < ssize(a.hallway); ++i) {
	if (a.hallway[i] != 0) {
            if (auto r = a.go_home(a.hallway[i], i)) {
		auto [next, price] = *r;
                if (auto s = solve(next, current_energy + price, local_best, seen, depth + 1)) {
                    ++local_solved;
                    local_best = std::min(local_best, *s);
                }
            }
	}
    }
    for (int b = 0; b < 4; ++b) {
        for (int i : {0, 1, 2, 3}) {
            auto antipod = a.burrows[b][i];
            auto should_move_out = [&]{
                for (int j = i; j < 4; ++j) {
                    if (b != a.dst_burrow_idx(a.burrows[b][j]))
                        return true;
                }
                return false;
            };
            if (antipod != 0 && should_move_out()) {
                a.go_out(antipod, b, i, [&](auto next, auto price) {
                    if (auto s = solve(next, current_energy + price, local_best, seen, depth + 1)) {
                        ++local_solved;
                        local_best = std::min(local_best, *s);
                    }
                });
            }
        }
    }
    return local_solved ? std::optional(local_best) : std::nullopt;
}

int main() {
    Antipodium input;
    {
	std::string line;
	std::getline(std::cin, line);
	std::getline(std::cin, line);
	std::string burrows[2];
	std::getline(std::cin, burrows[0]);
	std::getline(std::cin, burrows[1]);
	int j = 0;
	for (auto &bur : burrows) {
	    for (int i = 0; i < 4; ++i) {
		input.burrows[i][j] = bur[3 + 2 * i];
	    }
	    ++j;
	}

        for (int b : {0, 1, 2, 3}) {
            for (int i : {2, 3}) {
                input.burrows[b][i] = 'A' + b;
            }
        }
	input.dump(0);
    }

    std::map<Antipodium, long> seen;
    std::cout << solve(input, 0, std::numeric_limits<long>::max(), seen).value() << '\n';

    seen.clear();
    for (int i : {0, 1, 2, 3}) {
        input.burrows[i][3] = input.burrows[i][1];
    }
    input.burrows[0][1] = 'D';
    input.burrows[0][2] = 'D';
    input.burrows[1][1] = 'C';
    input.burrows[1][2] = 'B';
    input.burrows[2][1] = 'B';
    input.burrows[2][2] = 'A';
    input.burrows[3][1] = 'A';
    input.burrows[3][2] = 'C';

    input.dump(0);
    std::cout << solve(input, 0, std::numeric_limits<long>::max(), seen).value() << '\n';
}
