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
	out.emplace_back(val / 8);
	out.emplace_back((val % 8) / 4);
	out.emplace_back((val % 4) / 2);
	out.emplace_back(val % 2);
    }
    return out;
}

using BIt = BitVector::const_iterator;

long slice_num(BIt from, BIt to) {
    long out = 0;
    for (; from != to; ++from) {
	out *= 2;
	out += int(*from);
    }
    return out;
}

Packet decode(BIt &bit) {
    int version = slice_num(bit, bit + 3);
    int type = slice_num(bit + 3, bit + 6);
    bit += 6;

    if (type == 4) {
	long num = 0;
	while (*bit) {
	    num *= 16;
	    num += slice_num(bit + 1, bit + 5);
	    bit += 5;
	}
	num *= 16;
	num += slice_num(bit + 1, bit + 5);
	bit += 5;
	std::cerr << "lit " << version << ' ' << num << '\n';
	return Packet{version, num};
    }
    Packet p(version, type);
    std::cerr << "op " << version << ' ' << type << " (\n";
    if (!*bit) {
	long len = slice_num(bit + 1, bit + 16);
	std::cerr << "len = " << len << '\n';
	bit += 16;
	auto start = bit;
	while (bit - start < len) {
	    p.op.subs.emplace_back(std::make_unique<Packet>(decode(bit)));
	}
    }
    else {
	long num = slice_num(bit + 1, bit + 12);
	std::cerr << "num = " << num << '\n';
	bit += 12;
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

int main() {
    BitVector raw_packet = load(std::cin);
    /*
    for (int v : raw_packet) {
	std::cerr << v;
    }
    std::cerr << '\n';
    */
    auto packet = decode(raw_packet);

    long versions = 0;
    walk(packet, [&](int v, long) { versions += v; }, [&](int v, int) { versions += v; });
    std::cerr << versions << '\n';
}
