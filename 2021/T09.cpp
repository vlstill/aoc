#include <cassert>
#include <iostream>
#include <iterator>
#include <map>
#include <set>
#include <sstream>
#include <string>
#include <string_view>
#include <vector>

struct Map {

    Map(std::istream &is) {
        std::string line;
        while (std::getline(is, line)) {
            assert(rowlength == 0 || rowlength == line.size());
            rowlength = line.size();

            for (char c : line) {
                map.push_back(c - '0');
            }
        }
    }

    int operator[](std::pair<int, int> idx) const {
        return map[idx.second * rowlength + idx.first];
    }

    template< typename Yield >
    void neighbors(int x, int y, Yield yield) const {
        for (int dx : { -1, 0, 1 }) {
            for (int dy : { -1, 0, 1 }) {
                if (std::abs(dx) + std::abs(dy) == 1) {
                    int nx = x + dx;
                    int ny = y + dy;
                    if (0 <= nx && nx < cols() && 0 <= ny && ny < rows())
                        yield((*this)[{nx, ny}], nx, ny);
                }
            }
        }
    }

    template< typename Yield >
    void enumerate(Yield yield) const {
        for (int i = 0, s = map.size(); i < s; ++i) {
            yield(map[i], i % rowlength, i / rowlength);
        }
    }

    int rows() const { return map.size() / rowlength; }
    int cols() const { return rowlength; }

    std::vector<int> map;
    size_t rowlength = 0;
};

long get_basin_size(auto &map, int x₀, int y₀) {
    std::set<std::pair<int, int>> seen;
    auto go = [&](auto go, int x, int y) -> void {
        map.neighbors(x, y, [&](auto val, auto nx, auto ny) {
            if (val != 9 && !seen.contains({nx, ny})) {
                seen.emplace(nx, ny);
                go(go, nx, ny);
            }
        });
    };
    go(go, x₀, y₀);
    return seen.size();
}

int main() {
    Map map(std::cin);

    long risk = 0;
    std::multiset< int > basin_sizes;;
    map.enumerate([&](auto val, auto x, auto y) {
        bool is_min = true;
        map.neighbors(x, y, [&](auto nval, auto nx, auto ny) {
            is_min = is_min && nval > val;
        });
        if (is_min) {
            risk += val + 1;
            basin_sizes.insert(get_basin_size(map, x, y));
        }
    });
    std::cout << risk << '\n';
    auto back = std::prev(basin_sizes.end());
    assert(basin_sizes.size() >= 3);
    std::cout << *back * *std::prev(back) * *std::prev(back, 2) << '\n';
}
