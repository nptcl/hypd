#ifndef __FIXED_TEST_HEADER__
#define __FIXED_TEST_HEADER__

#define test(x, y) { if (test_execute((x) != 0, (y))) goto error; }
#define Return { return 0; error: return 1; }
#define TestCall(x) { if (x()) {test_error++; return 1; }}
#define FIXED_DEGRADE_WIDTH		70

extern int test_count;
extern int test_error;
extern int test_switch;
extern int test_position;
extern struct fixed_random random_state;

int test_execute(int check, const char *name);
void test_abort(void);

#endif

