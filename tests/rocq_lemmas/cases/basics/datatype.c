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

int main(void)
{
  return 0;
}