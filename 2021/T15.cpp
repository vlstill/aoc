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

struct Map {
    Map(int cols, int rows) : rowlength(cols) {
	map.resize(rows * cols);
    }

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

    long &operator[](std::pair<int, int> idx) {
        return map[idx.second * rowlength + idx.first];
    }

    template< typename Yield >
    void neighbors(int x, int y, Yield yield) {
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
    void enumerate(Yield yield) {
        for (int i = 0, s = map.size(); i < s; ++i) {
            yield(map[i], i % rowlength, i / rowlength);
        }
    }

    void dump(std::ostream &os = std::cerr) {
	int j = 0;
        for (auto val : map) {
            os << val << '|';
            ++j;
            if (j % rowlength == 0)
                os << '\n';
        }
        os << '\n';
    }

    int rows() const { return map.size() / rowlength; }
    int cols() const { return rowlength; }

    std::vector<long> map;
    size_t rowlength = 0;
};

void find_risk(Map map) {
    Map risk(map.cols(), map.rows());
    std::fill(risk.map.begin(), risk.map.end(), std::numeric_limits<long>::max() - 100);
    risk[{risk.cols() - 1, risk.rows() - 1}] = 0;

    bool changed = true;
    while (changed) {
	changed = false;
	for (int y = map.rows() - 1; y >= 0; --y) {
	    for (int x = map.cols() - 1; x >= 0; --x) {
		risk.neighbors(x, y, [&](auto &val, auto nx, auto ny) {
		    auto alt = map[{x, y}] + risk[{x, y}];
		    if (alt < risk[{nx, ny}]) {
			risk[{nx, ny}] = alt;
			changed = true;
		    }
		});
	    }
	}
    }
    std::cerr << risk[{0,0}] << '\n';
}

int main() {
    Map map(std::cin);
    find_risk(map);

    Map map2(map.cols() * 5, map.rows() * 5);
    map2.enumerate([&](auto &val, auto x, auto y) {
	val = map[{x % map.cols(), y % map.rows()}] + (x / map.cols()) + (y / map.rows());
	val = (val - 1) % 9 + 1;
    });
    find_risk(map2);
}
