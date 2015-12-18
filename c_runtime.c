#include <assert.h>
#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>
#include <time.h>

#define BASIC_CHANNEL_MEMBERS                                                  \
  pthread_mutex_t lock;                                                        \
  int size;                                                                    \
  bool poisoned;                                                               \
  pthread_cond_t write_ready;                                                  \
  pthread_cond_t read_ready;                                                   \
  int front;                                                                   \
  int back;                                                                    \
  int MAX_SIZE;                                                                \
  int claimed_for_writing;                                                     \
  int claimed_for_reading;                                                     \
  pthread_t writing_thread;                                                    \
  pthread_t reading_thread;

struct _channel {
  BASIC_CHANNEL_MEMBERS
};

struct _int_channel {
  BASIC_CHANNEL_MEMBERS
  int queue[100];
};

struct _char_channel {
  BASIC_CHANNEL_MEMBERS
  char queue[100];
};

struct _double_channel {
  BASIC_CHANNEL_MEMBERS
  double queue[100];
};

#define MALLOC_CHANNEL(type)                                                   \
  = (struct _##type##_channel *)malloc(sizeof(struct _##type##_channel));

int _init_channel(struct _channel *channel) {
  if (pthread_mutex_init(&channel->lock, NULL) != 0) {
    printf("Mutex init failed");
    return 1;
  }

  if (pthread_cond_init(&channel->write_ready, NULL) +
          pthread_cond_init(&channel->read_ready, NULL) !=
      0) {
    printf("Cond init failed");
    return 1;
  }
  channel->claimed_for_reading = 0;
  channel->claimed_for_writing = 0;
  channel->MAX_SIZE = 100;
  channel->front = 0;
  channel->back = 0;
  channel->poisoned = false;
  return 0;
}

struct _channel_list_node {
  struct _channel *chan;
  struct _channel_list_node *next;
};

struct _pthread_node {
  pthread_t thread;
  struct _pthread_node *next;
  char *proc_name;
  struct _channel_list_node *writing_channels;
};

struct _pthread_node *_head = NULL;
struct _pthread_node *_tail = NULL;

pthread_mutex_t _thread_list_lock;
pthread_mutex_t _ref_counting_lock;

struct _pthread_node *_get_thread(pthread_t thread_id) {
  pthread_mutex_lock(&_thread_list_lock);
  struct _pthread_node *curr = _head;
  while (curr) {
    if (curr->thread == thread_id) {
      break;
    }
    curr = curr->next;
  }
  pthread_mutex_unlock(&_thread_list_lock);
  return curr;
}

char *_get_thread_name(pthread_t thread_id) {
  if (_head == NULL)
    return "";
  pthread_mutex_lock(&_thread_list_lock);
  char *name = "";
  struct _pthread_node *curr = _head;
  while (curr) {
    if (curr->thread == thread_id) {
      name = curr->proc_name;
      break;
    }
    curr = curr->next;
  }
  pthread_mutex_unlock(&_thread_list_lock);
  return name;
}

void _print_dot_node(struct _channel *chan) {
  fprintf(stderr, "{%d[label=%s]}->{%d[label=%s]}\n", (int)chan->writing_thread,
          _get_thread_name(chan->writing_thread), (int)chan->reading_thread,
          _get_thread_name(chan->reading_thread));
}

#define MAKE_ENQUEUE_FUNC(type)                                                \
  type _enqueue_##type(type element, struct _##type##_channel *channel,        \
                       bool dot_print) {                                       \
    pthread_mutex_lock(&channel->lock);                                        \
    pthread_t this_thread = pthread_self();                                    \
    if (!channel->claimed_for_writing) {                                       \
      channel->claimed_for_writing = 1;                                        \
      channel->writing_thread = this_thread;                                   \
      struct _pthread_node *this_thread_node = _get_thread(this_thread);       \
      struct _channel_list_node *new_writing_chan =                            \
          malloc(sizeof(struct _channel_list_node));                           \
      new_writing_chan->next = this_thread_node->writing_channels;             \
      new_writing_chan->chan = (struct _channel *)channel;                     \
      this_thread_node->writing_channels = new_writing_chan;                   \
      if (channel->claimed_for_reading && dot_print)                           \
        _print_dot_node((struct _channel *)channel);                           \
    } else if (channel->writing_thread != this_thread) {                       \
      fprintf(stderr, "Runtime error: proc %s (thread 0x%x) is trying to "     \
                      "write to a channel belonging to %s (thread 0x%x)\n",    \
              _get_thread_name(this_thread), (int)this_thread,                 \
              _get_thread_name(channel->writing_thread),                       \
              (int)channel->writing_thread);                                   \
      exit(1);                                                                 \
    }                                                                          \
    while (channel->size >= channel->MAX_SIZE)                                 \
      pthread_cond_wait(&channel->write_ready, &channel->lock);                \
    assert(channel->size < channel->MAX_SIZE);                                 \
    if (channel->poisoned) {                                                   \
      fprintf(stderr,                                                          \
              "Attempting to read from a channel that is empty and poisoned"); \
      exit(1);                                                                 \
    }                                                                          \
    channel->queue[channel->back] = element;                                   \
    channel->back = (channel->back + 1) % channel->MAX_SIZE;                   \
    channel->size++;                                                           \
    pthread_cond_signal(&channel->read_ready);                                 \
    pthread_mutex_unlock(&channel->lock);                                      \
    return element;                                                            \
  }

MAKE_ENQUEUE_FUNC(int)
MAKE_ENQUEUE_FUNC(char)
MAKE_ENQUEUE_FUNC(double)

#define CALL_ENQUEUE_FUNC(e, c, t, dot) _enqueue_##t(e, c, dot)

#define MAKE_DEQUEUE_FUNC(type)                                                \
  type _dequeue_##type(struct _##type##_channel *channel, bool dot_print) {    \
    pthread_mutex_lock(&channel->lock);                                        \
    pthread_t this_thread = pthread_self();                                    \
    if (!channel->claimed_for_reading) {                                       \
      channel->claimed_for_reading = 1;                                        \
      channel->reading_thread = this_thread;                                   \
      if (channel->claimed_for_writing && dot_print)                           \
        _print_dot_node((struct _channel *)channel);                           \
    } else if (channel->reading_thread != this_thread) {                       \
      fprintf(stderr, "Runtime error: proc %s (thread 0x%x) is trying to "     \
                      "read from a channel belonging to %s (thread 0x%x)\n",   \
              _get_thread_name(this_thread), (int)this_thread,                 \
              _get_thread_name(channel->reading_thread),                       \
              (int)channel->reading_thread);                                   \
      exit(1);                                                                 \
    }                                                                          \
    if (channel->size == 0) {                                                  \
      fprintf(stderr, "Attempting to read from empty channel");                \
      exit(1);                                                                 \
    }                                                                          \
    type result = channel->queue[channel->front];                              \
    channel->front = (channel->front + 1) % channel->MAX_SIZE;                 \
    channel->size--;                                                           \
    pthread_cond_signal(&channel->write_ready);                                \
    pthread_mutex_unlock(&channel->lock);                                      \
    return result;                                                             \
  }

MAKE_DEQUEUE_FUNC(int)
MAKE_DEQUEUE_FUNC(char)
MAKE_DEQUEUE_FUNC(double)

#define CALL_DEQUEUE_FUNC(c, t, dot) _dequeue_##t(c, dot)

void _poison(struct _channel *channel) {
  pthread_mutex_lock(&channel->lock);
  channel->poisoned = true;
  pthread_cond_signal(&channel->read_ready);
  pthread_mutex_unlock(&channel->lock);
}

bool _wait_for_more(struct _channel *channel) {
  pthread_mutex_lock(&channel->lock);
  while (channel->size == 0) {
    if (channel->poisoned) {
      pthread_mutex_unlock(&channel->lock);
      return false;
    } else {
      pthread_cond_wait(&channel->read_ready, &channel->lock);
    }
  }
  pthread_mutex_unlock(&channel->lock);
  return true;
}

void _initialize_runtime(bool print_dot) {
  pthread_mutex_init(&_thread_list_lock, NULL);
  pthread_mutex_init(&_ref_counting_lock, NULL);
  srand(time(NULL));
  if (print_dot)
    fprintf(stderr, "digraph G{\n");
}

pthread_t *_make_pthread_t(char *proc_name) {
  pthread_mutex_lock(&_thread_list_lock);
  struct _pthread_node *new_pthread =
      (struct _pthread_node *)malloc(sizeof(struct _pthread_node));
  new_pthread->next = NULL;
  new_pthread->proc_name = proc_name;
  new_pthread->writing_channels = NULL;
  if (_head == NULL) {
    _head = _tail = new_pthread;
  } else {
    _tail->next = new_pthread;
    _tail = new_pthread;
  }
  pthread_mutex_unlock(&_thread_list_lock);
  return &(new_pthread->thread);
}

void _exit_thread() {
  struct _pthread_node *this_thread = _get_thread(pthread_self());
  struct _channel_list_node *curr_chan = this_thread->writing_channels;
  while (curr_chan) {
    if (!curr_chan->chan->poisoned)
      _poison(curr_chan->chan);
    curr_chan = curr_chan->next;
  }
  pthread_exit(NULL);
}

void _wait_for_finish(bool print_dot) {
  struct _pthread_node *curr = _head;
  while (curr) {
    pthread_join(curr->thread, NULL);
    curr = curr->next;
  }
  if (print_dot)
    fprintf(stderr, "}");
}

union _payload {
  int _int;
  double _double;
  char _char;
  void *_cell;
  struct _int_channel *_int_channel;
};

struct _cell {
  struct _cell *next;
  union _payload data;
  int references;
  int length;
};

struct _cell *_add_front(union _payload element, struct _cell *tail) {
  struct _cell *new_cell = malloc(sizeof(struct _cell));
  new_cell->references = 1;
  new_cell->data = element;
  new_cell->next = tail;
  if (!tail)
    new_cell->length = 1;
  else {
    new_cell->length = tail->length + 1;
    tail->references++;
  }
  return new_cell;
}

struct _cell *_get_tail(struct _cell *head) {
  if (!head) {
    fprintf(stderr, "Runtime error: cannot get tail of empty list");
    exit(1);
  }

  return head->next;
}

void __decrease_refs(struct _cell *head, int lock) {
  // if(lock)
  // pthread_mutex_lock(&_ref_counting_lock);
  // if(!head){
  // if(lock)
  // pthread_mutex_unlock(&_ref_counting_lock);
  // return;
  //}
  // else if(head->references > 1)
  // head->references--;
  // else{
  //__decrease_refs(head->next, 0);
  // free(head);
  //}
  // if(lock)
  // pthread_mutex_unlock(&_ref_counting_lock);
}

void _decrease_refs(struct _cell *head) {
  //__decrease_refs(head, 1);
}

void _increase_refs(struct _cell *head) {
  pthread_mutex_lock(&_ref_counting_lock);
  if (head)
    head->references++;
  pthread_mutex_unlock(&_ref_counting_lock);
}

union _payload _get_front(struct _cell *head) {
  if (!head) {
    fprintf(stderr, "Runtime error: cannot get head of empty list");
    exit(1);
  }

  return head->data;
}

int _get_length(struct _cell *head) {
  if (!head)
    return 0;
  return head->length;
}
