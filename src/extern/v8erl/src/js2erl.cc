#include "js2erl.h"
#include <string.h>

#define TERMS(terms, tlen) (terms != NULL ? (terms + tlen) : NULL)

// internal
static int date_to_terms(Handle<Date> val, ErlDrvTermData *terms, list<void *> *pl);
static int object_to_terms(Handle<Object> obj, ErlDrvTermData *terms, list<void *> *pl);
static int array_to_terms(Handle<Array> arr, ErlDrvTermData *terms, list<void *> *pl);
static int number_to_terms(Handle<Number> num, ErlDrvTermData *terms, list<void *> *pl);
static int string_to_terms(Handle<String> str, ErlDrvTermData *terms, list<void *> *pl);

static int atom(const char *a, ErlDrvTermData *terms) {
  if (terms != NULL) {
    terms[0] = ERL_DRV_ATOM;
    terms[1] = driver_mk_atom(const_cast<char *>(a));
  }
  return 2;
}

// public

int terms_length(Handle<Value> val) {
  return value_to_terms(val, NULL, NULL);
}

int value_to_terms(Handle<Value> val, ErlDrvTermData *terms, list<void *> *pl) {
  if (val->IsNull()) {
    //printf("[v2t] is null\n");
    return atom("null", terms);
  } else if (val->IsString()) {
    //printf("[v2t] is String\n");
    return string_to_terms(Handle<String>::Cast(val), terms, pl);
  } else if (val->IsNumber()) {
    //printf("[v2t] is Number\n");
    return number_to_terms(Handle<Number>::Cast(val), terms, pl);
  } else if (val->IsBoolean()) {
    //printf("[v2t] is Boolean\n");
    return atom(val->IsTrue() ? "true" : "false", terms);
  } else if (val->IsObject()) {
    //printf("[v2t] is Object\n");
    if (val->IsArray()) {
      //printf("[v2t] is Array\n");
      return array_to_terms(Handle<Array>::Cast(val), terms, pl);
    } else if (val->IsDate()) {
      //printf("[v2t] is Date\n");
      return date_to_terms(Handle<Date>::Cast(val), terms, pl);
    } else {
      return object_to_terms(Handle<Object>::Cast(val), terms, pl);
    }
  } else { // functions, etc.
    //printf("[v2t] is unknown\n");
    return atom("undefined", terms);
  }
}

// private

int array_to_terms(Handle<Array> arr, ErlDrvTermData *terms, list<void *> *pl) {
  int tlen = 0;

  int alen = arr->Length();
  for (int i = 0; i < alen; ++i) {
    tlen += value_to_terms(arr->Get(Integer::New(i)), TERMS(terms, tlen), pl);
  }
  
  if (terms != NULL) {
    terms[tlen] = ERL_DRV_NIL;
  }
  tlen++;

  if (alen > 0) {
    if (terms != NULL) {
      terms[tlen] = ERL_DRV_LIST;
      terms[tlen + 1] = alen + 1;
    }
    tlen += 2;
  }
  
  return tlen;
}

int object_to_terms(Handle<Object> obj, ErlDrvTermData *terms, list<void *> *pl) {
  //printf("[v2t] object_to_terms\n");
  int tlen = 0;

  tlen += atom("struct", TERMS(terms, tlen));
  
  Handle<Array> keys = obj->GetPropertyNames();
  int olen = keys->Length();
  for (int i=0; i<olen; ++i) {
    Handle<Value> key = keys->Get(Integer::New(i));
    tlen += string_to_terms(Handle<String>::Cast(key), TERMS(terms, tlen), pl);
    tlen += value_to_terms(obj->Get(key), TERMS(terms, tlen), pl);
    
    if (terms != NULL) {
      terms[tlen] = ERL_DRV_TUPLE;
      terms[tlen + 1] = 2;
    }
    tlen += 2;
  }

  if (terms != NULL) {
    terms[tlen] = ERL_DRV_NIL;
    terms[tlen + 1] = ERL_DRV_LIST;
    terms[tlen + 2] = olen + 1;
    terms[tlen + 3] = ERL_DRV_TUPLE; // {struct, [{<<"key">>, Value}, ...]}
    terms[tlen + 4] = 2;
  }
  tlen += 5;

  return tlen;
}

int number_to_terms(Handle<Number> num, ErlDrvTermData *terms, list<void *> *pl) {
  if (terms != NULL) {
    if (num->IsInt32()) {
      terms[0] = ERL_DRV_INT;
      terms[1] = num->Int32Value();
    } else {
      double *f = reinterpret_cast<double *>(driver_alloc(sizeof(double)));
      *f = num->Value();
      pl->push_back(f);
      terms[0] = ERL_DRV_FLOAT;
      terms[1] = reinterpret_cast<ErlDrvTermData>(f);
    }
  }
  return 2;
}

int string_to_terms(Handle<String> str, ErlDrvTermData *terms, list<void *> *pl) {
  if (terms != NULL) {
    String::Utf8Value utf8(str);
    size_t len = utf8.length();
    char *drv_str = reinterpret_cast<char *>(driver_alloc(len));
    memcpy(drv_str, *utf8, len);
    pl->push_back(drv_str);
    terms[0] = ERL_DRV_BUF2BINARY;
    terms[1] = reinterpret_cast<ErlDrvTermData>(drv_str);
    terms[2] = len;
  }
  return 3;
}

int date_to_terms(Handle<Date> date, ErlDrvTermData *terms, list<void *> *pl) {
  if (terms != NULL) {
    double *f = reinterpret_cast<double *>(driver_alloc(sizeof(double)));
    *f = date->NumberValue();
    pl->push_back(f);
    terms[0] = ERL_DRV_FLOAT;
    terms[1] = reinterpret_cast<ErlDrvTermData>(f);
  }
  return 2;
}
