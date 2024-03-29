#include <cassert>
#include <iostream>
#include <iterator>
#include <map>
#include <set>
#include <sstream>
#include <string>
#include <string_view>
#include <vector>

unsigned get_mask(std::string_view digit) {
    unsigned mask = 0;
    for (auto cseg : digit) {
        int iseg = cseg - 'a';
        mask |= 1u << iseg;
    }
    return mask;
}

const std::vector<std::string_view> segments { "abcefg", "cf", "acdeg", "acdfg", "bcdf", "abdfg", "abdefg", "acf", "abcdefg", "abcdfg" };

int decode_simple(std::string_view dig) {
    int pos = 0;
    int last = -1;
    for (size_t i = 0; i < 10; ++i) {
        if (segments[i].size() == dig.size()) {
            ++pos;
            last = i;
        }
    }
    if (pos == 1)
        return last;
    return -1;
}

std::string bin(unsigned val) {
    if (val == 0)
        return "0";
    std::string out;
    while (val) {
        out.push_back('0' + val % 2);
        val /= 2;
    }
    std::reverse(out.begin(), out.end());
    return out;
}

struct Decoder {
    Decoder(std::vector<std::string> digits) {
        std::fill(mapping.begin(), mapping.end(), 0b1111111);
        for (auto digit : digits) {
            prune(digit);
        }
    }

    void prune(std::string_view digit) {
        unsigned mask = 0;

        int candidates = 0;
        auto possible = _decode(digit);
        for (int i = 0; i < 10; ++i) {
            auto segs = segments[i];
            if (segs.size() == digit.size() && possible.contains(i)) {
                mask |= get_mask(segs);
                ++candidates;
            }
        }

        for (char brokenseg : digit)
            mapping[brokenseg - 'a'] &= mask;

        if (candidates == 1) {
            // off segments
            auto digmask = get_mask(digit);
            for (int i = 0; i < 8; ++i) {
                if (digmask & (1u << i))
                    continue;

                mapping[i] &= ~mask;
            }
        }
    }

    std::set<int> _decode(std::string_view digit) {
        if (auto s = decode_simple(digit); s >= 0)
            return {s};

        std::set<int> got;
        decrec(digit, "", [&got](int val) { got.insert(val); } );
        return got;
    }
    int decode(std::string_view digit) {
        auto got = _decode(digit);
        assert(got.size() == 1);
        return *got.begin();
    }

    template< typename Yield >
    void decrec(std::string_view digit, std::string accum, Yield yield) {
        if (digit.empty()) {
            std::sort(accum.begin(), accum.end());
            for (int i = 0; i < 10; ++i) {
                if (segments[i] == accum) {
                    yield(i);
                }
            }
            return;
        }

        int fst = digit[0] - 'a';
        digit.remove_prefix(1);

        int last = -1;
        for (unsigned i = 0; i < 8; ++i) {
            if (mapping[fst] & (1u << i)) {
                decrec(digit, accum + std::string{char('a' + i)}, yield);
            }
        }
    }

    std::array<unsigned, 7> mapping {};
};

int main() {
    std::string line;

    long easy = 0;
    long sum = 0;

    while (std::getline(std::cin, line)) {
        auto split = line.find('|');
        auto first = line.substr(0, split - 1);
        auto second = line.substr(split + 1);
        std::stringstream sf( first );
        std::stringstream ss( second );

        std::vector<std::string> digits(std::istream_iterator<std::string>{ss},
                                        std::istream_iterator<std::string>{});

        long accum = 0;
        Decoder dec(std::vector(std::istream_iterator<std::string>{sf},
                                std::istream_iterator<std::string>{}));
        for (auto dig : digits) {
            accum *= 10;
            if (decode_simple(dig) >= 0)
                ++easy;
            accum += dec.decode(dig);
        }
        sum += accum;
    }
    std::cout << easy << '\n';
    std::cout << sum << '\n';
}
