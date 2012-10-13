#!/usr/bin/env python

class Query(object):
    """
    A mongo query builder in the fluent style.
    All query constructs can be built. Once buillt, the query is available till the invocation of INIT.
    All operators are simply upper case versions of the actual operators. Except for elemMatch, which is ELEM_MATCH.
    This has limitation that these names are reserved, and cannot be used in the mongo document.
    Two synctatic elements - NEXT and END - are invented to deal with the allowed grammer.

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
    """

    ### public API
    def __init__(self, debug = False):
        self._debug = debug
        self._init()

    ### private parts
    class Frame(object):
        def __init__(self, state):
            self.state = state
            self.value = None

    class Event(object):
        OP_LOG = "op-log"
        OP_REL = "op-rel"
        OP_EXP = "op-exp"
        FIELD = "field"
        VALUE = "value"
        NEXT = "next"
        END = "end"

        FWD = "fwd"
        REV = "rev"

        def __init__(self, value = None, kind = None):
            self.kind = kind if kind else self.VALUE
            self.value = value
            self.dir = self.FWD

    def _init(self):
        self._stack = []
        self._frame = None
        self._state(self._exp)
        return self

    _end = Event(None, Event.END)

    def _query(self):
        while len(self._stack) > 1:
            self._event(self._end)
        return self._frame.value

    def _state(self, state, event = None):
        self._frame = self.Frame(state)
        self._stack.append(self._frame)
        self._frame.state(None, None)
        if event:
            self._event(event)

    def _event(self, ip):
        op = None
        while True:
            if self._debug:
                print("===== {0}".format(self._frame.state.__name__))
            if op:
                if self._debug:
                    print("          <-- op -- {0} [{1}]".format(op.value, op.kind))
                xx, op = self._frame.state(None, op)
            if not op and ip:
                if self._debug:
                    print("          <-- ip -- {0} [{1}]".format(ip.value, ip.kind))
                ip, op = self._frame.state(ip, None)
                if ip:
                    ip.dir = self.Event.REV
            if not (ip or op) or not self._pop():
                break

    def _pop(self):
        if len(self._stack) > 1:
            self._stack.pop()
            self._frame = self._stack[-1]
            return self._frame
        else:
            return None

    _op_exp = ["ELEM_MATCH"]
    _op_log = ["AND", "OR", "NOR", "NOT"]
    _op_rel = ["EQ", "NE", "GT", "GTE", "LT", "LTE", "ALL", "EXISTS", "MOD", "IN", "NIN", "SIZE", "TYPE", "REGEX"]
    _op_tok = ["NEXT", "END"]

    _op_exp = dict([(x, True) for x in _op_exp])
    _op_log = dict([(x, True) for x in _op_log])
    _op_rel = dict([(x, True) for x in _op_rel])
    _op_tok = dict([(x, True) for x in _op_tok])

    _op_nam = {}

    _op_nam["ELEM_MATCH"] = "elemMatch"

    def __getattr__(self, event):
        if event == "INIT":
            return self._init()
        elif event == "QUERY":
            return self._query()
        elif event in self._op_exp:
            kind = self.Event.OP_EXP
        elif event in self._op_log:
            kind = self.Event.OP_LOG
        elif event in self._op_rel:
            kind = self.Event.OP_REL
        elif event in self._op_tok:
            kind = event.lower()
        else:
            kind = self.Event.FIELD

        self._event(self.Event(event, kind))

        return self

    def __call__(self, *args):
        if len(args) < 1:
            event = None
        elif len(args) < 2:
            event = args[0]
        else:
            event = list(args)

        self._event(self.Event(event))

        return self

    def _err(self, event):
        print("\nerr: unexpected token {0}\n".format(event.value))
        raise AttributeError

    def _op(self, name):
        if name in self._op_nam:
            return "$" + self._op_nam[name]
        else:
            return "$" + name.lower()

    def _exp(self, ip, op):
        if ip:
            if ip.kind == self.Event.FIELD:
                self._state(self._field, ip)

            elif ip.kind == self.Event.OP_LOG:
                self._frame.field = self._op(ip.value)
                self._state(self._exp_list)

            else:
                return ip, self.Event(self._frame.value)

        elif op:
            if op.kind == self.Event.FIELD:
                self._frame.field = op.value
                self._state(self._exp_rhs)
            else:
                self._frame.value[self._frame.field] = op.value

        else:
            self._frame.value = {}

        return None, None

    def _exp_rhs(self, ip, op):
        if ip:
            if ip.kind == self.Event.VALUE:
                return None, ip

            elif ip.kind in (self.Event.OP_LOG, self.Event.OP_REL, self.Event.OP_EXP):
                self._state(self._op_list, ip)

            else:
                return ip, None

        elif op:
            return None, op

        return None, None

    def _exp_list(self, ip, op):
        if ip:
            if ip.kind == self.Event.END:
                return None, self.Event(self._frame.value)

            if ip.kind == self.Event.NEXT:
                self._state(self._exp)

            elif ip.dir == self.Event.FWD:
                self._state(self._exp, ip)

            else:
                return ip, self.Event(self._frame.value)

        elif op:
            self._frame.value.append(op.value)

        else:
            self._frame.value = []

        return None, None

    def _op_list(self, ip, op):
        if ip:
            if ip.value == "NOT":
                self._frame.op = self._op(ip.value)
                self._state(self._op_list)

            elif ip.kind == self.Event.OP_LOG:
                self._frame.op = self._op(ip.value)
                self._state(self._exp_list)

            elif ip.kind == self.Event.OP_REL:
                self._frame.op = self._op(ip.value)
                self._state(self._value)

            elif ip.kind == self.Event.OP_EXP:
                self._frame.op = self._op(ip.value)
                self._state(self._exp)

            else:
                return ip, self.Event(self._frame.value)

        elif op:
            self._frame.value[self._frame.op] = op.value

        else:
            self._frame.value = {}

        return None, None

    def _field(self, ip, op):
        if ip:
            if ip.kind == self.Event.FIELD:
                self._frame.value = self._frame.value + "." + ip.value if self._frame.value else ip.value

            else:
                return ip, self.Event(self._frame.value, self.Event.FIELD)

        return None, None

    def _value(self, ip, op):
        if ip:
            if ip.kind == self.Event.VALUE:
                return None, ip

            else:
                self._err(ip)

        return None, None

    def _tmpl(self, ip, op):
        if ip:
            pass

        elif op:
            pass

        else:
            pass

        return None, None
