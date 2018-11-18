/*
  CDE - C/C++ development environment for emacs
  Copyright (C) 2016 Oleg Keri

  This program is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program.  If not, see <http://www.gnu.org/licenses/>
*/

#define BOOST_TEST_MODULE TestParseInfo

#include <boost/test/unit_test.hpp>

#include "externalprocess.h"
#include <fileutil.cc>

std::string loadFromFile(const char *filename) {
    if (std::ifstream f(filename); f) {
        return std::string((std::istreambuf_iterator<char>(f)),
            std::istreambuf_iterator<char>());
    }
    return "";
}

BOOST_AUTO_TEST_CASE(TestParseInfo) {
    auto ppath =
        std::filesystem::path(ExternalProcess::selfPath()).parent_path();
    auto path = fileutil::join(ppath.string(), "parse_info/test.cpp");
    auto data = loadFromFile(path.c_str());
    BOOST_REQUIRE_EQUAL(data != "", true);
    BOOST_CHECK_EQUAL(fileutil::extractPosInString(data, 23, INVALID, 0), "Dos = 0x2");
    BOOST_CHECK_EQUAL(fileutil::extractPosInString(data, 70, INVALID, 0), "Zwei = 0x222");
    BOOST_CHECK_EQUAL(fileutil::extractPosInString(data, 193, INVALID, 0),
        "auto e1 = init(Dos + Zwei, -Tres)");
}
