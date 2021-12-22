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

const int X = 0, Y = 1, Z = 2;

struct Vector {
    Vector() : _data{0, 0, 0} { }
    Vector(long x, long y, long z) : _data{x, y, z} { }

    std::array<long, 3> _data;
    long x() const { return _data[0]; }
    long y() const { return _data[1]; }
    long z() const { return _data[2]; }

    long sum() const { return x() + y() + z(); }
    long asum() const { return std::abs(x()) + std::abs(y()) + std::abs(z()); }
    long amax() const { return std::max(std::abs(x()), std::max(std::abs(y()), std::abs(z()))); }

    Vector operator+(const Vector &v) const {
        return {x() + v.x(), y() + v.y(), z() + v.z()};
    }

    Vector operator*(long val) const {
        return {x() * val, y() * val, z() * val};
    }

    friend std::ostream &operator<<(std::ostream &out, const Vector &v) {
        out << '[' << v.x() << ',' << v.y() << ',' << v.z() << ']';
        return out;
    }
};

struct Point {
    Point() : _data{0, 0, 0} { }
    Point(long x, long y, long z) : _data{x, y, z} { }

    std::array<long, 3> _data;
    long x() const { return _data[0]; }
    long y() const { return _data[1]; }
    long z() const { return _data[2]; }

    Point operator+(const Vector &v) const {
        return {x() + v.x(), y() + v.y(), z() + v.z()};
    }

    Vector operator-(const Point &v) const {
        return {x() - v.x(), y() - v.y(), z() - v.z()};
    }

    auto operator<=>(const Point &) const = default;

    friend std::ostream &operator<<(std::ostream &out, const Point &v) {
        out << '(' << v.x() << ',' << v.y() << ',' << v.z() << ')';
        return out;
    }
};

struct Leaf {
    bool enabled = false;
};

template< typename OT >
struct Split_ {
    explicit Split_(Point p, bool val = false) : value(p) {
	for (auto &v : children) {
	    v = std::make_unique<OT>();
	    v->leaf().enabled = val;
	}
    }

    Point value;
    std::array<std::unique_ptr<OT>, 8> children;

    OT &child(bool dx, bool dy, bool dz) {
	auto idx = int(dx) | (int(dy) << 1) | (int(dz) << 2);
	return *children[idx];
    }
};

struct OctTree {
    OctTree() : data(Leaf()) { }

    std::variant<Split_<OctTree>, Leaf> data;

    bool is_leaf() const {
	return std::holds_alternative<Leaf>(data);
    }

    Leaf &leaf() { return std::get<Leaf>(data); }
    Split_<OctTree> &split() { return std::get<Split_<OctTree>>(data); }
};
using Split = Split_<OctTree>;

void part_1(const auto &ranges) {
    std::set<Point> init_points;

    for (auto [on, r] : ranges) {
	auto [rx, ry, rz] = r;
	auto [sx, ex] = rx;
	auto [sy, ey] = ry;
	auto [sz, ez] = rz;
	std::cerr << sx << "," << ex
 	   << ' ' << sy << "," << ey
 	   << ' ' << sz << "," << ez << '\n';
	for (long x = std::max(-50l, sx); x < std::min(51l, ex); ++x) {
	    for (long y = std::max(-50l, sy); y < std::min(51l, ey); ++y) {
		for (long z = std::max(-50l, sz); z < std::min(51l, ez); ++z) {
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

const std::array<bool, 3> T3 = {true, true, true};

std::ostream &operator<<(std::ostream &os, std::optional<long> v) {
    if (v.has_value())
	return os << *v;
    return os << "⊥";
}

using OptLong = std::optional<long>;
using Bound = std::pair<OptLong, OptLong>;
using Bounds = std::array<Bound, 3>;

long min(std::optional<long> a, long b) {
    if (a.has_value())
	return std::min(*a, b);
    return b;
}

long max(std::optional<long> a, long b) {
    if (a.has_value())
	return std::max(*a, b);
    return b;
}

void insert(auto &ot, bool val, Range r, Bounds bounds = {}, int prefix = 0) {
    auto [rx, ry, rz] = r;
    auto [sx, ex] = rx;
    auto [sy, ey] = ry;
    auto [sz, ez] = rz;
    Point upper_ex{ex, ey, ez};
    Point lower{sx, sy, sz};
    if (ot.is_leaf()) {
	auto tree = &ot;
	std::cerr << std::string(prefix, ' ') << "leaf ";
	if ((!bounds[0].first.has_value() || *bounds[0].first < sx)
	    || (!bounds[1].first.has_value() || *bounds[1].first < sy)
	    || (!bounds[2].first.has_value() || *bounds[2].first < sz))
	{
	    tree->data = Split{lower, tree->leaf().enabled};
	    tree = &tree->split().child(true, true, true);
	    std::cerr << "lsplit ";
	}
	if ((!bounds[0].second.has_value() || ex < *bounds[0].second)
	    || (!bounds[1].second.has_value() || ey < *bounds[1].second)
	    || (!bounds[2].second.has_value() || ez < *bounds[2].second))
	{
	    tree->data = Split{upper_ex, tree->leaf().enabled};
	    tree = &tree->split().child(false, false, false);
	    std::cerr << "usplit ";
	}
	std::cerr << "[" << bounds[0].first << ", " << bounds[0].second << ") × "
		  << "[" << bounds[1].first << ", " << bounds[1].second << ") × "
		  << "[" << bounds[2].first << ", " << bounds[2].second << ")";
	std::cerr << (val ? "⊤\n" : "⊥\n");
	tree->leaf().enabled = val;
	return;
    }

    std::cerr << std::string(prefix, ' ') << "split "
	      << "[" << sx << ", " << ex << ") × "
	      << "[" << sy << ", " << ey << ") × "
	      << "[" << sz << ", " << ez << ") by " << ot.split().value << "\n";
    auto &sp = ot.split();
    for (bool dx : {true, false}) {
	for (bool dy : {true, false}) {
	    for (bool dz : {true, false}) {
		auto new_ragne(r);
		auto new_bounds(bounds);
		std::array<bool, 3> ds = {dx, dy, dz};

		for (int i = 0; i < 3; ++i) {
		    if (ds[i]) {
			new_ragne[i].first = std::max(new_ragne[i].first, sp.value._data[i]);
			new_bounds[i].first = max(new_bounds[i].first, sp.value._data[i]);
		    }
		    else {
			new_ragne[i].second = std::min(new_ragne[i].second, sp.value._data[i]);
			new_bounds[i].second = min(new_bounds[i].second, sp.value._data[i]);
		    }
		}
		std::cerr << std::string(prefix, ' ') << "    into " << (dx ? "⊤" : "⊥") << (dy ? "⊤" : "⊥") << (dz ? "⊤ " : "⊥ ");
		bool empty = false;
		for (auto [s, e] : new_ragne) {
		    empty = empty || e <= s;
		    std::cerr << "[" << s << ", " << e << ") × ";
		}
		std::cerr << (empty ? "⊥\n" : "⊤\n");
		if (!empty)
		    insert(sp.child(dx, dy, dz), val, new_ragne, new_bounds, prefix + 4);
	    }
	}
    }
}

long sum_enabled(auto &ot, Bounds bounds = {}, int prefix = 0) {
    auto [rx, ry, rz] = bounds;
    auto [sx, ex] = rx;
    auto [sy, ey] = ry;
    auto [sz, ez] = rz;

    std::cerr << std::string(prefix, ' ') << "sum "
	      << "[" << sx << ", " << ex << ") × "
	      << "[" << sy << ", " << ey << ") × "
	      << "[" << sz << ", " << ez << ")\n";

    if (ot.is_leaf()) {
	if (ot.leaf().enabled) {
	    std::cerr << std::string(prefix + 4, ' ') << "leaf ⊤\n";
	    return (*ex - *sx) * (*ey - *sy) * (*ez - *sz);
	}
	else {
	    return 0;
	}
    }

    auto &sp = ot.split();
    long sum = 0;
    for (bool dx : {true, false}) {
	for (bool dy : {true, false}) {
	    for (bool dz : {true, false}) {
		auto new_bounds(bounds);
		std::array<bool, 3> ds = {dx, dy, dz};

		for (int i = 0; i < 3; ++i) {
		    if (ds[i])
			new_bounds[i].first = max(new_bounds[i].first, sp.value._data[i]);
		    else
			new_bounds[i].second = min(new_bounds[i].second, sp.value._data[i]);
		}
		bool empty = false;
		for (auto [s, e] : new_bounds)
		    empty = empty || (e.has_value() && s.has_value() && *e <= *s);
		if (!empty)
		    sum += sum_enabled(sp.child(dx, dy, dz), new_bounds, prefix + 4);
	    }
	}
    }
    return sum;
}

void part_2(const auto &ranges) {
    OctTree ot;
    for (auto [v, r] : ranges) {
	insert(ot, v, r);
    }
    std::cerr << sum_enabled(ot);
}

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
	    range[i] = {from, to + 1};
	    std::cerr << from << ',' << to << ' ';
	}
	std::cerr << '\n';
	ranges.emplace_back(on, range);
    }
    part_1(ranges);
    part_2(ranges);
}
