let boilerplate_header =
"
 #include <assert.h>
 #include <pthread.h>
 #include <stdio.h>
 #include <stdlib.h>
 #include <stdbool.h>

 #define BASIC_CHANNEL_MEMBERS pthread_mutex_t lock; \
                               int size; \
                               bool poisoned; \
                               pthread_cond_t write_ready; \
                               pthread_cond_t read_ready; \
                               int front; \
                               int back; \
                               int MAX_SIZE;

struct _channel{
  BASIC_CHANNEL_MEMBERS
};

struct _int_channel{
  BASIC_CHANNEL_MEMBERS
  int queue[100];
};

struct _char_channel{
  BASIC_CHANNEL_MEMBERS
  char queue[100];
};

#define MALLOC_CHANNEL(type) = (struct _##type##_channel*) malloc(sizeof(struct _##type##_channel));

int _init_channel(struct _channel *channel){
  if(pthread_mutex_init(&channel->lock, NULL) != 0){
      printf(\"Mutex init failed\");
      return 1;
  }

  if(pthread_cond_init(&channel->write_ready, NULL) +
          pthread_cond_init(&channel->read_ready, NULL ) != 0){
      printf(\"Cond init failed\");
      return 1;
  }
  channel->MAX_SIZE = 100;
  channel->front = 0;
  channel->back = 0;
  channel->poisoned = false;
  return 0;
}

#define MAKE_ENQUEUE_FUNC(type) void _enqueue_##type(type element, struct _##type##_channel *channel){ \
    pthread_mutex_lock(&channel->lock); \
    while(channel->size >= channel->MAX_SIZE) \
        pthread_cond_wait(&channel->write_ready, &channel->lock); \
    assert(channel->size < channel->MAX_SIZE); \
    assert(!(channel->poisoned)); \
    channel->queue[channel->back] = element; \
    channel->back = (channel->back + 1) % channel->MAX_SIZE; \
    channel->size++; \
    pthread_cond_signal(&channel->read_ready); \
    pthread_mutex_unlock(&channel->lock); \
}

MAKE_ENQUEUE_FUNC(int)
MAKE_ENQUEUE_FUNC(char)

#define SELECT_QUEUEING_FUNC(x) _Generic((x), \
    int: _enqueue_int, \
    char: _enqueue_char, \
    struct _int_channel*: _dequeue_int)

//#define CALL_ENQUEUE_FUNC(e, c, t) SELECT_QUEUEING_FUNC(e)(e, c)

#define CALL_ENQUEUE_FUNC(e, c, t) _enqueue_##t(e, c)

#define MAKE_DEQUEUE_FUNC(type) type _dequeue_##type(struct _##type##_channel *channel){ \
    pthread_mutex_lock(&channel->lock); \
    assert(channel->size != 0); \
    type result = channel->queue[channel->front]; \
    channel->front = (channel->front + 1) % channel->MAX_SIZE; \
    channel->size--; \
    pthread_cond_signal(&channel->write_ready); \
    pthread_mutex_unlock(&channel->lock); \
    return result; \
}

MAKE_DEQUEUE_FUNC(int)
MAKE_DEQUEUE_FUNC(char)

#define CALL_DEQUEUE_FUNC(c, t) _dequeue_##t(c)

void _poison(struct _channel * channel) {
    pthread_mutex_lock(&channel->lock);
    channel->poisoned = true;
    pthread_cond_signal(&channel->read_ready);
    pthread_mutex_unlock(&channel->lock);
}

bool _wait_for_more(struct _channel *channel) {
    pthread_mutex_lock(&channel->lock);
    while(channel->size == 0) {
        if(channel->poisoned){
            pthread_mutex_unlock(&channel->lock);
            return false;
        }
        else {
            pthread_cond_wait(&channel->read_ready, &channel->lock);
        }
    }
    pthread_mutex_unlock(&channel->lock);
    return true;
}

struct _pthread_node{
  pthread_t thread;
  struct _pthread_node *next;
};

struct _pthread_node* _head = NULL;

pthread_mutex_t _thread_list_lock;

pthread_t* _make_pthread_t() {
  pthread_mutex_lock(&_thread_list_lock);
  struct _pthread_node *new_pthread = (struct _pthread_node *) malloc(sizeof(struct _pthread_node));
  new_pthread->next = _head;
  _head = new_pthread;
  pthread_mutex_unlock(&_thread_list_lock);
  return &(new_pthread->thread);
}

void _wait_for_finish(){
  struct _pthread_node* curr = _head;
  while(curr){
    pthread_join(curr->thread, NULL);
    curr = curr->next;
  }
}\n
"
