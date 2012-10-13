#!/usr/bin/env python

import os
import sys

sys.path.append(os.path.abspath(os.path.join(os.path.dirname(__file__), '../query')))

import query

query = query.Query(False)

print( 0, query.INIT.aa("aaa").bb("bbb").QUERY)
print( 1, query.INIT.aa.EQ("aaa").LT("aaa").bb.LTE("bbb").QUERY)
print( 2, query.INIT.aa.EQ("aaa").AND.aa.EQ("aaa").bb.LTE("bbb").END.cc.EQ("ccc").QUERY)
print( 3, query.INIT.aa.EQ("aaa").bb.AND.aa.EQ("aaa").bb.LTE("bbb").END.cc.EQ("ccc").QUERY)
print( 4, query.INIT.aa.EQ("aaa").AND.aa.EQ("aaa").bb.LTE("bbb").NEXT.cc.EQ("ccc").GT("ccc").END.cc.EQ("ccc").QUERY)
print( 5, query.INIT.aa.EQ("aaa").NOT.AND.aa.EQ("aaa").bb.LTE("bbb").NEXT.cc.EQ("ccc").GT("ccc").END.cc.EQ("ccc").QUERY)
print( 6, query.INIT.aa.EQ("aaa").bb.ELEM_MATCH.aa.EQ("aaa").bb("bbb").END.cc.EQ("ccc").QUERY)
print( 7, query.INIT.aa.ALL([1, 2, 3]).QUERY)
print( 8, query.INIT.aa.ALL(1, 2, 3).QUERY)
