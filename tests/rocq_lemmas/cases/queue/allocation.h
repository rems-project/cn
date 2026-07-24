extern struct queue *malloc_queue();
/*@ spec malloc_queue();
    requires true;
    ensures take R = Block<struct queue>(return);
@*/ 

extern void free_queue (struct queue *p);
/*@ spec free_queue(pointer p);
    requires take P = Block<struct queue>(p);
    ensures true;
@*/

extern struct queue_cell *malloc_queue_cell();
/*@ spec malloc_queue_cell();
    requires true;
    ensures take R = Block<struct queue_cell>(return);
@*/ 

extern void free_queue_cell (struct queue_cell *p);
/*@ spec free_queue_cell(pointer p);
    requires take P = Block<struct queue_cell>(p);
    ensures true;
@*/
