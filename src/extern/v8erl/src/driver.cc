#include <string>
#include "js2erl.h"

//#define DEBUG

#ifdef DEBUG
#define LOG(X) printf(X)
#else
#define LOG(X)
#endif

static ErlDrvTermData v8erl_driver_mk_atom(const char *atom) {
  return driver_mk_atom(const_cast<char *>(atom));
}

static char DRIVER_NAME[] = "v8erl";
static const ErlDrvTermData OK = v8erl_driver_mk_atom("ok");
static const ErlDrvTermData EXCEPTION = v8erl_driver_mk_atom("exception");

static Persistent<ObjectTemplate> global_template;

class PortContext {
public:
  ErlDrvPort port;
  Persistent<Context> context;

  PortContext(ErlDrvPort);
  ~PortContext();
};

PortContext::PortContext(ErlDrvPort p) {
  port = p;

  HandleScope handle_scope;
  context = Context::New(NULL, global_template);
};

PortContext::~PortContext() {
  context.Dispose();
};

class ScriptRunner {
public:
  // input
  ErlDrvPort port;
  Persistent<Context> context;
  const char *buf;
  long len;
  long key;
  ErlDrvTermData caller;
  // output
  ErlDrvTermData type;
  Persistent<Value> result;

  ScriptRunner(ErlDrvPort, Persistent<Context>, const char *, long, long);
  void setResult(ErlDrvTermData atom, Handle<Value>);
  int send();
  ~ScriptRunner();
};

ScriptRunner::ScriptRunner(ErlDrvPort p, Persistent<Context> c, const char *b, long l, long k) {
  port = p;
  context = c;
  buf = b;
  len = l;
  key = k;
  caller = driver_caller(p);
};

ScriptRunner::~ScriptRunner() {
  delete buf;
  result.Dispose();
};

void ScriptRunner::setResult(ErlDrvTermData t, Handle<Value> r) {
  type = t;
  result = Persistent<Value>::New(r);
};

int ScriptRunner::send() {
  Context::Scope context_scope(context);
  HandleScope handle_scope;

  unsigned int tlen = terms_length(result) + 8;
  ErlDrvTermData *terms = reinterpret_cast<ErlDrvTermData *>(driver_alloc(sizeof(ErlDrvTermData) * tlen));

  terms[0] = ERL_DRV_UINT;
  terms[1] = key;
  terms[2] = ERL_DRV_ATOM;
  terms[3] = type;

  list<void *> pl;
  value_to_terms(result, terms + 4, &pl);

  terms[tlen - 4] = ERL_DRV_TUPLE;
  terms[tlen - 3] = 2;
  terms[tlen - 2] = ERL_DRV_TUPLE;
  terms[tlen - 1] = 2;

  int ret = driver_send_term(port, caller, terms, tlen);

  for (list<void *>::iterator iter = pl.begin() ; iter != pl.end(); ++iter) {
    driver_free(*iter);
  }
  driver_free(terms);

  return ret;
};

static int v8erl_init() {
  LOG("[drv] init\n");

  HandleScope handle_scope;
  global_template = Persistent<ObjectTemplate>::New(ObjectTemplate::New());
  return 0;
}

static void v8erl_finish() {
  global_template.Dispose();
}

static ErlDrvData v8erl_start(ErlDrvPort port, char *cmd) {
  LOG("[drv] start\n");
  return reinterpret_cast<ErlDrvData>(new PortContext(port));
}

static void v8erl_stop(ErlDrvData driver_data) {
  delete reinterpret_cast<PortContext *>(driver_data);
}

void run_script(void *async_data) {
  ScriptRunner *script_runner = reinterpret_cast<ScriptRunner *>(async_data);
 
  Context::Scope context_scope(script_runner->context);
  HandleScope handle_scope;
  TryCatch try_catch;

  Handle<String> script = String::New(script_runner->buf, script_runner->len);
  Handle<Script> compiled = Script::Compile(script);
  if (compiled.IsEmpty()) {
    script_runner->setResult(EXCEPTION, try_catch.Exception());
  } else {
    Handle<Value> value = compiled->Run();
    if (value.IsEmpty()) {
      script_runner->setResult(EXCEPTION, try_catch.Exception());
    } else {
      script_runner->setResult(OK, value);
    }
  }
}

void cancel_script_runner(void *async_data) {
  delete reinterpret_cast<ScriptRunner *>(async_data);
}

static void v8erl_output(ErlDrvData driver_data, char *buf, int len) {
  LOG("[drv] v8erl_output\n");
  PortContext *data = reinterpret_cast<PortContext *>(driver_data);

  int index = 0;
  int version;
  if (ei_decode_version(buf, &index, &version)) {
    LOG("[drv] bad version\n");
    return;
  }

  int arity;
  if (ei_decode_tuple_header(buf, &index, &arity) || (arity != 2)) {
    LOG("[drv] no tuple or wrong arity\n");
    return;
  }

  long key;
  if (ei_decode_long(buf, &index, &key)) {
    LOG("[drv] no key or wrong type\n");
    return;
  }

  int type;
  long script_length;
  if (ei_get_type(buf, &index, &type, reinterpret_cast<int *>(&script_length))) {
    LOG("[drv] cannot read size\n");
    return;
  }

  char *script = new char[script_length];
  if (ei_decode_binary(buf, &index, script, &script_length)) {
    LOG("[drv] bad script binary\n");
    delete script;
    return;
  }
  
  ScriptRunner *script_runner = new ScriptRunner(data->port, data->context, script, script_length, key);
  driver_async(data->port, NULL, run_script, script_runner, cancel_script_runner);
}

static void v8erl_ready_async(ErlDrvData driver_data, ErlDrvThreadData thread_data) {
  LOG("[drv] v8erl_ready_async\n");
  ScriptRunner *script_runner = reinterpret_cast<ScriptRunner *>(thread_data);
  script_runner->send();
  delete script_runner;
}

static ErlDrvEntry driver_entry = {
  v8erl_init, // init driver
  v8erl_start, // open_port
  v8erl_stop, // close_port
  v8erl_output, // output		
  NULL, // ready_input
  NULL, // ready_output
  DRIVER_NAME, // driver name
  v8erl_finish, // unload dynamic driver
  NULL, // handle: EMULATOR USE ONLY
  NULL, // control
  NULL, // timeout
  NULL, // outputv
  v8erl_ready_async, // async event done
  NULL, // flush
  NULL, // call
  NULL // event
};

extern "C" DRIVER_INIT(v8erl) {
  return &driver_entry;
}
