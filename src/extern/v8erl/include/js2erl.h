// -*- mode: c++ -*-
#include <list>
#include <v8.h>

#include "erl_driver.h"
#include "erl_interface.h"
#include "ei.h"

using namespace std;
using namespace v8;

extern int terms_length(Handle<Value> val);
extern int value_to_terms(Handle<Value> val, ErlDrvTermData *terms, list<void *> *);
