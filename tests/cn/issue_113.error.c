// regression test: see https://github.com/rems-project/cn/issues/113#issuecomment-2967000854
void f(char *p, unsigned x)
/*@
requires is_null(array_shift<unsigned char>(array_shift<unsigned char>(p, x), 1));
@*/
{
}