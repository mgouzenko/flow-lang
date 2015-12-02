#include <stdlib.h>
#include <stdio.h>

union _payload{
	int _int;
	double _double;
	char _char;
	void * _cell;
};

struct _cell {
	struct _cell *next;
	union _payload data;
	int references;
	int length;
};

struct _cell* _start_new_list(union _payload element){
	struct _cell *new_list = malloc(sizeof(struct _cell));
	new_list->references = 1;
	new_list->data = element;
	new_list->next = NULL;
	new_list->length = 1;
	return new_list;
}

struct _cell* add_front(union _payload element, struct _cell *tail){
	struct _cell *new_cell = malloc(sizeof(struct _cell));
	new_cell->references = 1;
	new_cell->data = element;
	new_cell->next = tail;
	new_cell->length = tail->length + 1;
	tail->references++;
	return new_cell;
}

struct _cell* get_tail(struct _cell* head){
	if(!head){
		printf("Runtime error: cannot get tail of empty list");
		exit(1);
	}

	return head->next;
}

union _payload _get_front(struct _cell* head){
	return head->data;
}

int _get_length(struct _cell* head){
	if(!head) return 0;
	return head->length;
}
