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

struct Vector {
    Vector() : _data{0, 0, 0} { }
    Vector(int x, int y, int z) : _data{x, y, z} { }

    std::array<int, 3> _data;
    int x() const { return _data[0]; }
    int y() const { return _data[1]; }
    int z() const { return _data[2]; }

    int sum() const { return x() + y() + z(); }
    int asum() const { return std::abs(x()) + std::abs(y()) + std::abs(z()); }
    int amax() const { return std::max(std::abs(x()), std::max(std::abs(y()), std::abs(z()))); }

    Vector operator+(const Vector &v) const {
        return {x() + v.x(), y() + v.y(), z() + v.z()};
    }

    Vector operator*(int val) const {
        return {x() * val, y() * val, z() * val};
    }

    friend std::ostream &operator<<(std::ostream &out, const Vector &v) {
        out << '[' << v.x() << ',' << v.y() << ',' << v.z() << ']';
        return out;
    }
};

struct Point {
    Point() : _data{0, 0, 0} { }
    Point(int x, int y, int z) : _data{x, y, z} { }
    explicit Point(Vector v) : _data(v._data) { }

    std::array<int, 3> _data;
    int x() const { return _data[0]; }
    int y() const { return _data[1]; }
    int z() const { return _data[2]; }

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

struct Orientation {
    Orientation() : _cache(_xyz()) { }

    std::tuple<Vector, std::pair<int, int>, int> _x() const {
        switch (_state % 6) {
            case 0: return {{ 1,  0,  0}, {1, 2}, 0};
            case 1: return {{-1,  0,  0}, {1, 2}, 0};
            case 2: return {{ 0,  1,  0}, {0, 2}, 1};
            case 3: return {{ 0, -1,  0}, {0, 2}, 1};
            case 4: return {{ 0,  0,  1}, {0, 1}, 2};
            case 5: return {{ 0,  0, -1}, {0, 1}, 2};
        }
        __builtin_unreachable();
    }

    std::tuple<Vector, Vector, int, int, int> _xy() const {
        auto [vx, rx, px] = _x();
        auto [a, b] = rx;

        auto base = (_state / 6) % 4;
        auto heading = base % 2 ? -1 : 1;
        Vector out;
        if (base / 2) {
            out._data[b] = heading;
            return {vx, out, a, px, b};
        }
        else {
            out._data[a] = heading;
            return std::tuple{vx, out, b, px, a};
        }
    }

    std::tuple<Vector, Vector, Vector> _xyz() const {
        auto [vx, vy, pz, px, py] = _xy();

        Vector z;
        z._data[pz] = vx.sum() * vy.sum() * (py < px ? -1 : 1) * (pz < py ? -1 : 1) * (pz < px ? -1 : 1);

        return {vx, vy, z};
    }

    friend std::ostream &operator<<(std::ostream &out, const Orientation &ori) {
        auto [x, y, z] = ori.xyz();
        out << '{' << x << ',' << y << ',' << z << '}';
        return out;
    }

    Point normalize(Point p) const {
        auto [vx, vy, vz] = xyz();
        auto [x, y, z] = p._data;
        Vector revec = vx * x + vy * y + vz * z;
        return {revec.x(), revec.y(), revec.z()};
    }

    void set_state(int s) {
        _state = s;
        _cache = _xyz();
    }

    std::tuple<Vector, Vector, Vector> xyz() const { return _cache; }

  private:
    int _state = 0;
    std::tuple<Vector, Vector, Vector> _cache;
};

struct Scanner {
    Scanner(std::istream &in) {
        std::string line;
        while (std::getline(in, line) && !line.empty()) {
            std::string sx, sy, sz;
            std::stringstream ss(line);
            std::getline(ss, sx, ',');
            std::getline(ss, sy, ',');
            std::getline(ss, sz);

            Point p{std::stoi(sx), std::stoi(sy), std::stoi(sz)};
            auto [_, ins] = _beacons.insert(p);
            assert(ins);
        }
    }

    Point normalize(Point p) const {
        return Point(axes.normalize(p) + origin);
    }

    std::set<Point> beacons() {
        std::set<Point> out;
        for (auto &b : _beacons) {
            out.insert(normalize(b));
        }
        return out;
    }

    std::set<Point> _beacons;
    bool done = false;
    Orientation axes;
    Vector origin;
};

int main() {
    std::vector<Scanner> scans;

    std::string line;
    while (std::getline(std::cin, line) && !line.empty() && line[0] == '-') {
        scans.emplace_back(std::cin);
    }
    scans[0].done = true;

    int done = 0;
    int last = 0;
    do {
        for (int j = 0; j < ssize(scans); ++j) {
            for (int i = 1; i < ssize(scans); ++i) {
                auto &origin = scans[j];
                auto &target = scans[i];
                if (i == j || !origin.done || target.done)
                    continue;

                int maxcnt = 0;
                for (int o = 0; o < 24; ++o) {
                    target.axes.set_state(o);

                    auto obeacons = origin.beacons();
                    for (auto &borig : obeacons) {
                        target.origin = {0, 0, 0};
                        for (auto &btarg : target.beacons()) {
                            auto diff = borig - btarg;
                            target.origin = diff;
                            int cnt = 0;
                            for (auto &bt : target.beacons()) {
                                cnt += obeacons.contains(bt)
                                    && (bt - Point(origin.origin)).amax() <= 1000
                                    && (bt - Point(target.origin)).amax() <= 1000;
                            }
                            maxcnt = std::max(maxcnt, cnt);
                            if (cnt >= 12) {
                                std::cerr << "intersected " << j + 1 << " & " << i + 1 << " (" << i + 1 << " at " << target.origin << ")\n";
                                goto done;
                            }
                        }
                    }
                }
                continue;

              done:
                target.done = true;
            }
        }
        last = done;
        done = 0;
        for (auto &scanner : scans) {
            if (scanner.done)
                ++done;
        }
        std::cerr << "pass: " << done << " done of " << scans.size() << "\n";
    } while (done != ssize(scans) && done != last);

    std::set<Point> beacons;
    for (auto &scanner : scans) {
        for (auto &b : scanner.beacons()) {
            beacons.insert(b);
        }
    }
    std::cout << beacons.size() << '\n';


    int max = 0;
    for (auto &sa : scans) {
        for (auto &sb : scans) {
            max = std::max((Point(sa.origin) - Point(sb.origin)).asum(), max);
        }
    }
    std::cout << max << '\n';
}
