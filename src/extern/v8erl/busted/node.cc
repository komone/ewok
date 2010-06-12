#include <stdlib.h>

#include "erl_interface.h"
#include "ei.h"

int main(int argc, char *argv[]) {
  int   id       = atoi(argv[0]);
  char *cookie   = argv[1];
  int   creation = atoi(argv[2]);
  char *node     = argv[3];

  erl_init(NULL, 0);
  
  int r = 0;

  if (!erl_connect_init(id, cookie, creation)) { erl_err_quit("erl_connect_init"); }
    
  int sock = erl_connect(node);
  if (sock < 0) { erl_err_quit("erl_connect"); }

  unsigned char buf[1024*16];
  ErlMessage emsg;
  while (true) {
    switch(erl_receive_msg(sock, buf, sizeof(buf), &emsg)) {
    case ERL_MSG:
      ETERM *resp = erl_mk_atom("ok");
      erl_send(sock, emsg.from, resp);
      erl_free_term(resp);
      break;
    case ERL_ERROR:
      // cleanup
      break;
    }
  }

  return r;
}
