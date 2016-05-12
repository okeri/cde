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

#define BOOST_TEST_MODULE TestFileUtil

#include <boost/test/unit_test.hpp>
#include "../src/fileutil.h"

#ifdef _WIN32
    const std::string prefix("C:\\");
#else
    const std::string prefix("/home/");
#endif


BOOST_AUTO_TEST_CASE(TestFilenameSeparatorFunctions) {
    std::string path;

    // deleteTrailingSep
    path = prefix + "somedir" + SEPARATOR;
    fileutil::deleteTrailingSep(&path);
    BOOST_CHECK_EQUAL(prefix + "somedir", path);

    path = prefix + "somedir" + SEPARATOR + SEPARATOR;
    fileutil::deleteTrailingSep(&path);
    BOOST_CHECK_EQUAL(prefix + "somedir", path);

    path = prefix + "somedir";
    fileutil::deleteTrailingSep(&path);
    BOOST_CHECK_EQUAL(prefix + "somedir", path);

#ifndef _WIN32
    // check fs-root '/' handles correcly
    path = SEPARATOR;
    std::string cmp(path);
    fileutil::deleteTrailingSep(&path);
    BOOST_CHECK_EQUAL(cmp, path);
#endif

    // addTrailingSep
    path = prefix + "somedir";
    fileutil::addTrailingSep(&path);
    BOOST_CHECK_EQUAL(prefix + "somedir" + SEPARATOR, path);

    path = prefix + "somedir" + SEPARATOR;
    fileutil::addTrailingSep(&path);
    BOOST_CHECK_EQUAL(prefix + "somedir" + SEPARATOR, path);

    // dirUp
    path = prefix + "somedir" + SEPARATOR;
    BOOST_CHECK_EQUAL(prefix , fileutil::dirUp(path));

    path = prefix + "somedir";
    BOOST_CHECK_EQUAL(prefix , fileutil::dirUp(path));

    // purify
    path = prefix + "extra1" + SEPARATOR + "extra2" + SEPARATOR + "extra3" +
            SEPARATOR + ".." + SEPARATOR + ".." + SEPARATOR + ".." +
            SEPARATOR + "data";
    BOOST_CHECK_EQUAL(prefix + "data", fileutil::purify(path));
}

BOOST_AUTO_TEST_CASE(TestFilenameExtensionFunctions) {
    // basenameNoExt
    BOOST_CHECK_EQUAL("somefile", fileutil::basenameNoExt("somefile"));
    BOOST_CHECK_EQUAL("somefile", fileutil::basenameNoExt(prefix +
                                                          "somefile.ext"));

    // extension
    BOOST_CHECK_EQUAL(".ext", fileutil::extension(prefix + "somefile.ext"));
    BOOST_CHECK_EQUAL(".ext", fileutil::extension("somefile.ext"));
    BOOST_CHECK_EQUAL("", fileutil::extension(prefix + "somefile"));
    BOOST_CHECK_EQUAL("", fileutil::extension(prefix + "some.file" +
                                              SEPARATOR + "somefile"));
}

BOOST_AUTO_TEST_CASE(TestLineInFile) {
    std::string filename("simple");
    filename += SEPARATOR;
    filename += "simple.cpp";

    BOOST_CHECK_EQUAL("array[0].run1(42);", fileutil::findLineInFile(
        filename, 139));
    BOOST_CHECK_EQUAL("array[0].r", fileutil::findLineInFile(filename, 174));
    BOOST_CHECK_EQUAL("array[0].r", fileutil::findLineInFile(filename, 184));
}
