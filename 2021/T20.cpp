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

using Point = std::pair<long, long>;

struct Image {
    Image(std::istream &is) {
        std::string line;
	for (int i = 0; std::getline(is, line); ++i) {
	    _max_x = ssize(line) - 1;
	    for (int j = 0; j < ssize(line); ++j) {
		if (line[j] == '#') {
		    _data.emplace(j, i);
		}
	    }
	    _max_y = i;
	}
    }

    void step(std::vector<bool> &enhancement) {
	assert(ssize(enhancement) == 512);
	std::set<Point> next;

	for (long y = _min_y - 1; y <= _max_y + 1; ++y) {
	    for (long x = _min_x - 1; x <= _max_x + 1; ++x) {
		long val = 0;
		for (int dy : {-1, 0, 1}) {
		    for (int dx : {-1, 0, 1}) {
			auto vx = x + dx;
			auto vy = y + dy;
			val *= 2;
			int v = ((vx < _min_x || vx > _max_x || vy < _min_y || vy > _max_y) && outside) || _data.contains({vx, vy});
			val += v;
		    }
		}
		if (enhancement[val])
		    next.emplace(x, y);
	    }
	}

	--_min_x;
	--_min_y;
	++_max_x;
	++_max_y;
	_data = next;
	outside = outside ? enhancement.back() : enhancement.front();
    }

    void dump() {
	for (long y = _min_y; y <= _max_y; ++y) {
	    for (long x = _min_x; x <= _max_x ; ++x) {
		std::cerr << (_data.contains({x, y}) ? '#' : '.');
	    }
	    std::cerr << '\n';
	}
	std::cerr << '\n';
    }


    std::set<Point> _data;
    long _min_x = 0, _min_y = 0, _max_x = 0, _max_y = 0;
    bool outside = false;
};

int main() {
    std::vector<bool> enhancement;

    std::string line;
    std::getline(std::cin, line);
    for (auto v : line) {
	enhancement.emplace_back(v == '#');
    }
    std::getline(std::cin, line);
    Image img(std::cin);
    img.dump();

    for (int i : {1, 2}) {
	img.step(enhancement);
	img.dump();
    }
    std::cout << img._data.size() << '\n';

    for (int i = 2; i < 50; ++i) {
	img.step(enhancement);
	std::cerr << '.' << std::flush;
    }
    std::cerr << '\n';
    std::cout << img._data.size() << '\n';
}
