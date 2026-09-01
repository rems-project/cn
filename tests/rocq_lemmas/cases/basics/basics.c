/* 1. test case for the combination of C struct and CN data type */
struct data {
  int x;
};

/*@
datatype data_option {
  Data_none {},
  Data_some { struct data value }
}

lemma data_option_trivial (datatype data_option x)
  requires true;
  ensures true;
@*/


/* 2. test case for sizeof and offsetof */
struct stct {
  int first;
  int second;
};

/*@
lemma sizeof_offsetof_lemma ()
  requires true;
  ensures
    let x = sizeof<struct stct>;
    let y = offsetof(stct, second);
    y < x;
@*/

int main(void)
{
  return 0;
}