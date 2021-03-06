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

#define BOOST_TEST_MODULE TestExternal

#include <chrono>
#include <fstream>

#include <boost/filesystem/operations.hpp>
#include <boost/filesystem/path.hpp>
#include <boost/test/unit_test.hpp>

#include "externalprocess.h"

BOOST_AUTO_TEST_CASE(TestExternalSimple) {
    // init executable path
    auto ppath =
        boost::filesystem::path(ExternalProcess::selfPath()).parent_path();
    auto cdepath = ppath.parent_path() / boost::filesystem::path("cde");
    auto path = ppath.string() + boost::filesystem::path::preferred_separator;
    std::string srcfilename(path + "simple" +
                            boost::filesystem::path::preferred_separator +
                            "simple.cpp");

    // init prject ack from server
    std::string ack("(cde--ack \"");
    ack += srcfilename + "\" \"" + path + "simple\")\n";

    // create cache dir
    std::string cache(path + "cache");
    boost::filesystem::create_directory(cache);

    ExternalProcess cde;
    BOOST_REQUIRE_EQUAL(
        cde.open(cdepath.c_str(), (std::string("-C") + cache).c_str()), true);

    // ack
    cde.send(std::string("A ") + srcfilename + "\n");
    BOOST_CHECK_EQUAL(cde.recv(), ack);
    BOOST_CHECK_EQUAL(cde.recv(),
        std::string("(cde--hideif \"") + srcfilename + "\" '((9 10)))\n");
    BOOST_CHECK_EQUAL(cde.recv(), "(cde--error-rep 1)\n");
    // complete
    std::chrono::time_point<std::chrono::high_resolution_clock> start, end;
    start = std::chrono::system_clock::now();
    cde.recv();
    for (unsigned i = 0; i < 10; ++i) {
        cde.send(
            std::string("C ") + path + "simple " + srcfilename + " p 6 10\n");
        BOOST_CHECK_EQUAL(cde.recv(),
            "(cde--handle-completions '((\"void pop_back()\" 5 13)"
            "(\"void push_back(const value_type &__x)\" 5 14)"
            "(\"void push_back(value_type &&__x)\" 5 14)))\n");
    }
    end = std::chrono::system_clock::now();
    unsigned time =
        std::chrono::duration_cast<std::chrono::milliseconds>(end - start)
            .count();

    BOOST_TEST_MESSAGE("complete done in " << time << " ms");

    // quit
    cde.send("Q\n");
    BOOST_CHECK_EQUAL(cde.recv(), "(setq cde--process nil)\n");
    cde.close();
    // restore index
    BOOST_REQUIRE_EQUAL(
        cde.open(cdepath.c_str(), (std::string("-C") + cache).c_str()), true);

    std::string hdrfilename(path + "simple" +
                            boost::filesystem::path::preferred_separator +
                            "simple.h");
    // ack
    ack = std::string("(cde--ack \"") + hdrfilename + "\" \"" + path +
          "simple\")\n";
    cde.send(std::string("A ") + hdrfilename + "\n");
    BOOST_CHECK_EQUAL(cde.recv(), ack);
    BOOST_CHECK_EQUAL(cde.recv(), std::string("(cde--hideif \"") + hdrfilename +
                                      "\" '((10 11)(21 22)))\n");

    BOOST_CHECK_EQUAL(cde.recv(), "(cde--error-rep 1)\n");

    // hdr/src.
    cde.send(std::string("F ") + path + "simple " + hdrfilename + "\n");
    cde.recv();
    BOOST_CHECK_EQUAL(cde.recv(), "(find-file \"" + srcfilename + "\")\n");

    // def
    cde.send(std::string("D ") + path + "simple " + srcfilename + " 148\n");
    cde.recv();  // skip searchig
    BOOST_CHECK_EQUAL(
        cde.recv(), std::string("(find-file \"") + hdrfilename +
                        "\")(goto-char (point-min))(forward-char 390)(push "
                        "(list \"" +
                        srcfilename + "\" 147) cde--ring)(message \"\")\n");

    // ref
    cde.send(std::string("R ") + path + "simple " + hdrfilename + " 391\n");
    cde.recv();  // skip searching
    BOOST_CHECK_EQUAL(
        cde.recv(), std::string("(cde--ref-setup '((\"") + srcfilename +
                        "\" 7 \"array[0].run1(42);\")))(message \"\")\n");

    // multi mapping test
    srcfilename = path + "map" + boost::filesystem::path::preferred_separator +
                  "test.cpp";
    hdrfilename =
        path + "map" + boost::filesystem::path::preferred_separator + "test.h";

    std::ifstream fsrc(srcfilename);
    std::string src((std::istreambuf_iterator<char>(fsrc)),
        std::istreambuf_iterator<char>());

    std::ifstream fhdr(hdrfilename);
    std::string hdr((std::istreambuf_iterator<char>(fhdr)),
        std::istreambuf_iterator<char>());

    fsrc.close();
    fhdr.close();

    cde.send(std::string("A ") + srcfilename + "\n");
    cde.recv();
    cde.recv();
    cde.recv();
    cde.send(std::string("A ") + hdrfilename + "\n");
    cde.recv();
    cde.recv();
    cde.recv();

    cde.send(std::string("M ") + srcfilename + " " +
             std::to_string(src.length()) + "\n" + src);
    cde.send(std::string("M ") + hdrfilename + " " +
             std::to_string(hdr.length()) + "\n" + hdr);

    cde.send(std::string("B ") + path + "map " + srcfilename + "\n");
    BOOST_CHECK_EQUAL(cde.recv(), "(cde--error-rep 1)\n");

    cde.send(std::string("B ") + path + "map " + hdrfilename + "\n");
    BOOST_CHECK_EQUAL(cde.recv(), "(cde--error-rep 1)\n");

    // quit
    cde.send("Q\n");
    BOOST_CHECK_EQUAL(cde.recv(), "(setq cde--process nil)\n");
}
