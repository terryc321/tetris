
/* just want a key press detection that does not block
 or atleast getch
 */

#include <ncurses.h>
#include <libguile.h>

SCM getch_wrapper ()
{
  return scm_from_int (getch); 
}



void init_getch ()
{
  /* getch name of function want to extract
     0 = no formal args 
     0 = no optional args
     0 = rest arguments
     getch_wrapper = thing above
   */
  scm_c_define_gsubr ("getch", 0, 0, 0, getch_wrapper);
}




