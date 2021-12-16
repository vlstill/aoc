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

using BitVector = std::vector<bool>;
struct Literal {
    Literal(int v, int t, long p) : version(v), type(t), payload(p) { }

    int version;
    int type;
    long payload;
};
union Packet;
struct Operator {
    Operator(int v, int t) : version(v), type(t) { }

    int version;
    int type;
    std::vector<std::unique_ptr<Packet>> subs;
};

union Packet {
    Packet() : version(0), type(0) { }
    Packet(int version, long payload) : l(version, 4, payload) { }
    Packet(int version, int type) : op(version, type) { }

    Packet(Packet &&other) {
	if (other.type == 4) {
	    new ( &l ) Literal(other.l);
	}
	else {
	    new ( &op ) Operator(std::move(other.op));
	}
    }

    ~Packet() {
	if (type == 4) {
	    l.~Literal();
	} else {
	    op.~Operator();
	}
    }

    struct { int version; int type; };
    Literal l;
    Operator op;
};

template< typename T = void >
auto walk(Packet &p, auto lit, auto op) {
    if (p.type == 4) {
	return lit(p.version, p.l.payload);
    }
    if constexpr (std::is_same_v<T, void>) {
	for (auto &s : p.op.subs) {
	    walk<T>(*s, lit, op);
	}
	return op(p.version, p.type);
    } else {
	std::vector<T> subres;
	for (auto &s : p.op.subs) {
	    subres.emplace_back(walk<T>(*s, lit, op));
	}
	return op(p.version, p.type, subres);
    }
}

BitVector load(std::istream &is) {
    BitVector out;
    char ch;
    while (is.get(ch)) {
	int val;
	if ('A' <= ch && ch <= 'F') {
	    val = 10 + (ch - 'A');
	} else {
	    val = ch - '0';
	}
	for (int i = 16; i >= 2; i /= 2) {
	    out.emplace_back((val % i) / (i / 2));
	}
    }
    return out;
}

using BIt = BitVector::const_iterator;

long consume_num(BIt &from, int len) {
    long out = 0;
    for (; len > 0; --len, ++from) {
	out *= 2;
	out += int(*from);
    }
    return out;
}

Packet decode(BIt &bit) {
    int version = consume_num(bit, 3);
    int type = consume_num(bit, 3);

    if (type == 4) {
	long num = 0;
	while (consume_num(bit, 1)) {
	    num *= 16;
	    num += consume_num(bit, 4);
	}
	num *= 16;
	num += consume_num(bit, 4);
	std::cerr << "lit " << version << ' ' << num << '\n';
	return Packet{version, num};
    }
    Packet p(version, type);
    std::cerr << "op " << version << ' ' << type << " (\n";
    if (!consume_num(bit, 1)) {
	long len = consume_num(bit, 15);
	std::cerr << "len = " << len << '\n';
	auto start = bit;
	while (bit - start < len) {
	    p.op.subs.emplace_back(std::make_unique<Packet>(decode(bit)));
	}
    }
    else {
	long num = consume_num(bit, 11);
	std::cerr << "num = " << num << '\n';
	for (int i = 0; i < num; ++i) {
	    p.op.subs.emplace_back(std::make_unique<Packet>(decode(bit)));
	}
    }
    std::cerr << ")\n";
    return p;
}

Packet decode(const BitVector &bits) {
    auto start = bits.begin();
    return decode(start);
}

auto fold(auto &what, auto base, auto acc) {
    return std::accumulate(what.begin(), what.end(), base, acc);
}

auto fold1(auto &what, auto acc) {
    return std::accumulate(std::next(what.begin()), what.end(), *what.begin(), acc);
}

int main() {
    BitVector raw_packet = load(std::cin);
    auto packet = decode(raw_packet);

    long versions = 0;
    walk(packet, [&](int v, long) { versions += v; }, [&](int v, int) { versions += v; });
    std::cout << versions << '\n';

    std::cout << walk<long>(packet, [](int, long val) { return val; },
	  [](int, int t, auto subs) {
	      switch (t) {
		  case 0: return fold(subs, 0l, [](auto a, auto b) { return a + b; });
		  case 1: return fold(subs, 1l, [](auto a, auto b) { return a * b; });
		  case 2: return fold1(subs, [](auto a, auto b) { return std::min(a, b); });
		  case 3: return fold1(subs, [](auto a, auto b) { return std::max(a, b); });
		  case 5: return long(subs[0] > subs[1]);
		  case 6: return long(subs[0] < subs[1]);
		  case 7: return long(subs[0] == subs[1]);
		  default: __builtin_unreachable();
	      }
	  }) << '\n';
}
