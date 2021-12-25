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

using Map = std::vector<std::string>;

std::pair<bool, Map> step(const Map &map, char who, int dx, int dy) {
    bool moved = false;
    Map copy(map);
    for (int y = 0; y < ssize(map); ++y) {
        for (int x = 0; x < ssize(map[y]); ++x) {
            if (map[y][x] == who) {
                auto nx = (x + dx) % ssize(map[y]);
                auto ny = (y + dy) % ssize(map);

                if (map[ny][nx] == '.') {
                    copy[y][x] = '.';
                    copy[ny][nx] = who;
                    moved = true;
                }
            }
        }
    }
    return {moved, copy};
}

int main() {
    std::vector<std::string> map;
    std::string line;
    while (std::getline(std::cin, line))
        map.emplace_back(std::move(line));

    int i = 1;
    for (bool moved = true; moved; ++i) {
        std::tie(moved, map) = step(map, '>', 1, 0);
        bool m2;
        std::tie(m2, map) = step(map, 'v', 0, 1);
        moved |= m2;

        for (auto line : map)
            std::cerr << line << '\n';
        std::cerr << '\n';
    }
    std::cout << i - 1 << '\n';
}
