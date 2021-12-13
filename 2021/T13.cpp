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

struct Map {
    Map(int rows, int cols) : rowlength(cols) {
        map.resize(rows * cols);
    }

    Map(std::istream &is) {
        std::string line;
        std::vector<std::pair<int, int>> points;
        int max_x = 0, max_y = 0;
        while (std::getline(is, line) && !line.empty()) {
            std::stringstream ss(line);
            int x, y;
            char _comma;
            ss >> x >> _comma >> y;
            points.emplace_back(x, y);
            max_x = std::max(max_x, x);
            max_y = std::max(max_y, y);
        }

        map.resize((max_x + 1) * (max_y + 1));
        rowlength = max_x + 1;
        for (auto [x, y] : points) {
            (*this)[{x, y}] = 1;
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

    void dump(std::ostream &out = std::cerr) {
        int j = 0;
        for (auto val : map) {
            out << (val ? '#' : '.');
            ++j;
            if (j % rowlength == 0)
                out << '\n';
        }
        out << '\n';
    }

    Map fold(char axis, int fold) {
        assert(axis == 'x' || axis == 'y');
        Map out(axis == 'x' ? rows() : fold, axis == 'x' ? fold : cols());
        enumerate([&](auto val, auto x, auto y) {
            if ((axis == 'x' && x <= fold) || (axis == 'y' && y <= fold)) {
                out[{x, y}] |= val;
            } else {
                if (axis == 'x')
                    out[{fold - (x - fold), y}] |= val;
                else
                    out[{x, fold - (y - fold)}] |= val;
            }
        });
        return out;
    }

    int rows() const { return map.size() / rowlength; }
    int cols() const { return rowlength; }

    std::vector<int> map;
    size_t rowlength = 0;
};

int main() {
    Map paper(std::cin);

    std::string line;
    for (int i = 0; std::getline(std::cin, line); ++i) {
        auto eq = line.find('=');
        auto axis = line[eq - 1];
        auto val = std::stoi(line.substr(eq + 1));
        paper = paper.fold(axis, val);

        if (i == 0) {
            long visible_1 = 0;
            paper.enumerate([&](auto v, auto, auto) {
                visible_1 += bool(v);
            });
            std::cout << visible_1 << '\n';
        }
    }
    paper.dump(std::cout);
}
