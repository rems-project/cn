struct queue {
  struct queue_cell* front;  
  struct queue_cell* back;
};

struct queue_cell {
  int first;  
  struct queue_cell* next;
};
