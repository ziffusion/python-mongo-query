Python Mongo Fluent Query Builder
=================================

    A mongo query builder in the fluent style.

    All query constructs can be built. Once buillt, the query is available till the invocation of INIT.

    All operators are simply upper case versions of the actual operators. Except for elemMatch, which is ELEM_MATCH.

    This has limitation that these names are reserved, and cannot be used in the mongo document.

    Two synctatic elements - NEXT and END - are invented to deal with the allowed grammer.

Usage
=====

    query = Query()

    query.INIT.aa("aaa").bb("bbb").QUERY
    {'aa': 'aaa', 'bb': 'bbb'}

    query.INIT.aa.EQ("aaa").LT("aaa").bb.LTE("bbb").QUERY
    {'aa': {'$eq': 'aaa', '$lt': 'aaa'}, 'bb': {'$lte': 'bbb'}}

    query.INIT.aa.EQ("aaa").AND.aa.EQ("aaa").bb.LTE("bbb").END.cc.EQ("ccc").QUERY
    {'aa': {'$eq': 'aaa', '$and': [{'aa': {'$eq': 'aaa'}, 'bb': {'$lte': 'bbb'}}]}, 'cc': {'$eq': 'ccc'}}

    query.INIT.aa.EQ("aaa").bb.AND.aa.EQ("aaa").bb.LTE("bbb").END.cc.EQ("ccc").QUERY
    {'aa': {'$eq': 'aaa'}, 'cc': {'$eq': 'ccc'}, 'bb': {'$and': [{'aa': {'$eq': 'aaa'}, 'bb': {'$lte': 'bbb'}}]}}

    query.INIT.aa.EQ("aaa").AND.aa.EQ("aaa").bb.LTE("bbb").NEXT.cc.EQ("ccc").GT("ccc").END.cc.EQ("ccc").QUERY
    {'aa': {'$eq': 'aaa', '$and': [{'aa': {'$eq': 'aaa'}, 'bb': {'$lte': 'bbb'}}, {'cc': {'$eq': 'ccc', '$gt': 'ccc'}}]}, 'cc': {'$eq': 'ccc'}}

    query.INIT.aa.EQ("aaa").NOT.AND.aa.EQ("aaa").bb.LTE("bbb").NEXT.cc.EQ("ccc").GT("ccc").END.cc.EQ("ccc").QUERY
    {'aa': {'$eq': 'aaa', '$not': {'$and': [{'aa': {'$eq': 'aaa'}, 'bb': {'$lte': 'bbb'}}, {'cc': {'$eq': 'ccc', '$gt': 'ccc'}}]}}, 'cc': {'$eq': 'ccc'}}

    query.INIT.aa.EQ("aaa").bb.ELEM_MATCH.aa.EQ("aaa").bb("bbb").END.cc.EQ("ccc").QUERY
    {'aa': {'$eq': 'aaa'}, 'cc': {'$eq': 'ccc'}, 'bb': {'$elemMatch': {'aa': {'$eq': 'aaa'}, 'bb': 'bbb'}}}

    query.INIT.aa.ALL([1, 2, 3]).QUERY
    {'aa': {'$all': [1, 2, 3]}}

    query.INIT.aa.ALL(1, 2, 3).QUERY
    {'aa': {'$all': [1, 2, 3]}}
