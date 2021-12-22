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

using Range = std::array<std::pair<long, long>, 3>;

struct Point {
    Point() : _data{0, 0, 0} { }
    Point(int x, int y, int z) : _data{x, y, z} { }

    std::array<int, 3> _data;
    int x() const { return _data[0]; }
    int y() const { return _data[1]; }
    int z() const { return _data[2]; }

    auto operator<=>(const Point &) const = default;

    friend std::ostream &operator<<(std::ostream &out, const Point &v) {
        out << '(' << v.x() << ',' << v.y() << ',' << v.z() << ')';
        return out;
    }
};

int main() {
    std::string line;

    std::vector<std::pair<bool, Range>> ranges;

    while (std::getline(std::cin, line)) {
	std::stringstream ss(line);
	bool on = line[1] == 'n';

	std::string part;
	std::getline(ss, part, ' ');
	std::array<std::pair<long, long>, 3> range;
	for (int i = 0; i < 3; ++i) {
	    std::getline(ss, part, ',');
	    auto dot = part.find('.');
	    long from = std::stol(part.substr(2, dot));
	    long to = std::stol(part.substr(dot + 2));
	    range[i] = {from, to};
	    std::cerr << from << ',' << to << ' ';
	}
	std::cerr << '\n';
	ranges.emplace_back(on, range);
    }

    std::set<Point> init_points;

    for (auto [on, r] : ranges) {
	auto [rx, ry, rz] = r;
	auto [sx, ex] = rx;
	auto [sy, ey] = ry;
	auto [sz, ez] = rz;
	std::cerr << sx << "," << ex
 	   << ' ' << sy << "," << ey
 	   << ' ' << sz << "," << ez << '\n';
	for (long x = std::max(-50l, sx); x <= std::min(50l, ex); ++x) {
	    for (long y = std::max(-50l, sy); y <= std::min(50l, ey); ++y) {
		for (long z = std::max(-50l, sz); z <= std::min(50l, ez); ++z) {
		    if (on)
			init_points.emplace(x, y, z);
		    else
			init_points.erase(Point{x, y, z});
		}
	    }
	}
    }
    std::cout << init_points.size() << '\n';
}
