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

    int &operator[](std::pair<int, int> idx) {
        return map[idx.second * rowlength + idx.first];
    }

    template< typename Yield >
    void neighbors(int x, int y, Yield yield) {
        for (int dx : { -1, 0, 1 }) {
            for (int dy : { -1, 0, 1 }) {
                if (dx != 0 || dy != 0) {
                    int nx = x + dx;
                    int ny = y + dy;
                    if (0 <= nx && nx < cols() && 0 <= ny && ny < rows())
                        yield((*this)[{nx, ny}], nx, ny);
                }
            }
        }
    }

    template< typename Yield >
    void enumerate(Yield yield) {
        for (int i = 0, s = map.size(); i < s; ++i) {
            yield(map[i], i % rowlength, i / rowlength);
        }
    }

    int rows() const { return map.size() / rowlength; }
    int cols() const { return rowlength; }

    std::vector<int> map;
    size_t rowlength = 0;
};



int main() {
    long flashes = 0;
    Map map(std::cin);

    for (int i = 1; ; ++i) {
        // step
        std::deque<std::pair<int, int>> flash;
        map.enumerate([&](auto &val, auto x, auto y) {
            ++val;
            if (val == 10) {
                flash.emplace_back(x, y);
            }
        });
        while (!flash.empty()) {
            auto [x, y] = flash.front();
            flash.pop_front();
            ++flashes;
            map.neighbors(x, y, [&](auto &val, auto nx, auto ny) {
                ++map[{nx, ny}];
                if (map[{nx, ny}] == 10)
                    flash.emplace_back(nx, ny);
            });
        }
        map.enumerate([&](auto &val, auto, auto) {
            if (val >= 10)
                val = 0;
        });
        if (i == 100)
            std::cout << flashes << std::endl;
        if (std::accumulate(map.map.begin(), map.map.end(), 0l, [](long a, long b) { return a + b; }) == 0) {
            std::cout << i << '\n';
            break;
        }
    }
}
