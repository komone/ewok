#include <string>
#include <map>

#include "erl_interface.h"
#include "ei.h"

#include <v8.h>

using namespace std;
using namespace v8;

// templates

static Persistent<ObjectTemplate> global_template;
static Persistent<ObjectTemplate> term_template;

// term wrapper

Handle<Object> term_wrap(Handle<ObjectTemplate> temp, ETERM *term) {
  HandleScope scope;

  Handle<Object> obj = term_template->NewInstance();
  Handle<External> ext = External::New(term);
  obj->SetInternalField(0, ext);

  return scope.Close(obj);
}

ETERM *term_unwrap(Handle<Value> val) {
  Handle<Object> obj = Handle<Object>::Cast(val);
  Handle<External> ext = Handle<External>::Cast(obj->GetInternalField(0));
  void *ptr = ext->Value();
  return static_cast<ETERM *>(ptr);
}

// contexts

Persistent<Context> context_new() {
  Handle<Context> context = ;
  return ;
}

ETERM *context_execute(Handle<Context> context, char *scriptstr) {
  Context::Scope context_scope(context);
  HandleScope handle_scope;
  TryCatch try_catch;

  Handle<String> script = String::New(scriptstr);
  Handle<Script> compiled = Script::Compile(script);
  if (compiled.IsEmpty()) { 
    // TODO: throw some erlang exception
    return NULL; // XXX
  } else {
    Handle<Value> result = compiled->Run();
    if (result.IsEmpty()) {
      // TODO: throw some erlang exception
      return NULL; // XXX
    } else {
      // TODO: convert JS response to term
      return NULL; // XXX
    }
  }
}

// API

/*
static Handle<Array> erl_tuple_to_array(ETERM *tuple) {
  int size = ERL_TUPLE_SIZE(tuple);
  Handle<Array> array = Array::New(size);
  for (int i=0; i<size; i++) {
    array->Set(Integer::New(i), External::New(ERL_TUPLE_ELEMENT(TERM, i)));
  }
  return array;
}

static Handle<Array> erl_list_to_array(ETERM *list) {
  Handle<Array> array = Array::New(erl_length(list));
  int i = 0;
  while(!ERL_IS_NIL(list)) {
    array->Set(Integer::New(i), External::New(ERL_CONS_HEAD(list)));
    list = ERL_CONS_TAIL(list);
  }
  return array;
}
*/

Handle<Value> v8erl_type(const Arguments& args) {
  if (args.Length() < 1) { return v8::Undefined(); }

  ETERM *term = term_unwrap(args[0]);
  if (term == NULL) { return v8::Undefined(); }
  
  HandleScope scope;
  switch (ERL_TYPE(term)) {
  case ERL_UNDEF:       return scope.Close(String::New("undefined"));
  case ERL_INTEGER:     return scope.Close(String::New("int"));
  case ERL_U_INTEGER:   return scope.Close(String::New("uint"));
  case ERL_ATOM:        return scope.Close(String::New("atom"));
  case ERL_PID:         return scope.Close(String::New("pid"));
  case ERL_PORT:        return scope.Close(String::New("port"));
  case ERL_REF:         return scope.Close(String::New("ref"));
  case ERL_CONS:        // fall-through
  case ERL_NIL:         return scope.Close(String::New("list"));
  case ERL_TUPLE:       return scope.Close(String::New("tuple"));
  case ERL_BINARY:      return scope.Close(String::New("binary"));
  case ERL_FLOAT:       return scope.Close(String::New("float"));
  case ERL_VARIABLE:    return scope.Close(String::New("var"));
  case ERL_SMALL_BIG:   return scope.Close(String::New("sbigint"));
  case ERL_U_SMALL_BIG: return scope.Close(String::New("usbigint"));
  case ERL_FUNCTION:    return scope.Close(String::New("fun"));
  case ERL_BIG:         return scope.Close(String::New("bigint"));
  default:              return v8::Undefined(); // should never get here
  }
}


//
// MAIN
//

Persistent<ObjectTemplate> erl_global() {
  Handle<ObjectTemplate> global = ObjectTemplate::New(); 
  
  global->Set(String::New("term_type"),
              FunctionTemplate::New(v8erl_type));
 
  return Persistent<ObjectTemplate>::New(global);
}


Persistent<ObjectTemplate> erl_term_template() {
  Handle<ObjectTemplate> temp = ObjectTemplate::New();
  temp->SetInternalFieldCount(1);
  return Persistent<ObjectTemplate>::New(temp);
}

int main(int argc, char *argv[]) {
  // erlang init
  erl_init(NULL, 0);
  
  // v8 init
  global_template = erl_global();
  term_template   = erl_term_template();
  
  // main loop
  
  return 0;
}

static void cleanup() {
  global_template.Dispose();
  term_template.Dispose();
}
