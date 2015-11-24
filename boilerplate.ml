let boilerplate_header =
"
 #include <assert.h>
 #include <pthread.h>
 #include <stdio.h>
 #include <stdlib.h>
 #include <stdbool.h>

struct _int_channel{
  int queue[100];
  int front;
  int back;     // One past the last element
  int MAX_SIZE;
  int size;
  bool poisoned;
  pthread_mutex_t lock;
  pthread_cond_t write_ready;
  pthread_cond_t read_ready;
};


struct _char_channel{
    char queue[100];
    int front;
    int back;     // One past the last element
    int MAX_SIZE;
    int size;
    bool poisoned;
    pthread_mutex_t lock;
    pthread_cond_t write_ready;
    pthread_cond_t read_ready;
};

int _init_int_channel(struct _int_channel *channel){
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

int _init_char_channel(struct _char_channel *channel){
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

void _enqueue_int(int element, struct _int_channel *channel){
    pthread_mutex_lock(&channel->lock);
    while(channel->size >= channel->MAX_SIZE)
        pthread_cond_wait(&channel->write_ready, &channel->lock);

    assert(channel->size < channel->MAX_SIZE);
    assert(!(channel->poisoned));

    channel->queue[channel->back] = element;
    channel->back = (channel->back + 1) % channel->MAX_SIZE;

    channel->size++;
    pthread_cond_signal(&channel->read_ready);
    pthread_mutex_unlock(&channel->lock);
}

void _enqueue_char(char element, struct _char_channel *channel){
    pthread_mutex_lock(&channel->lock);
    while(channel->size >= channel->MAX_SIZE)
        pthread_cond_wait(&channel->write_ready, &channel->lock);

    assert(channel->size < channel->MAX_SIZE);
    assert(!(channel->poisoned));

    channel->queue[channel->back] = element;
    channel->back = (channel->back + 1) % channel->MAX_SIZE;

    channel->size++;
    pthread_cond_signal(&channel->read_ready);
    pthread_mutex_unlock(&channel->lock);
}

int _dequeue_int(struct _int_channel *channel){
    pthread_mutex_lock(&channel->lock);
    assert(channel->size != 0);

    int result = channel->queue[channel->front];
    channel->front = (channel->front + 1) % channel->MAX_SIZE;

    channel->size--;
    pthread_cond_signal(&channel->write_ready);
    pthread_mutex_unlock(&channel->lock);
    return result;
}

char _dequeue_char(struct _char_channel *channel){
    pthread_mutex_lock(&channel->lock);
    assert(channel->size != 0);

    char result = channel->queue[channel->front];
    channel->front = (channel->front + 1) % channel->MAX_SIZE;

    channel->size--;
    pthread_cond_signal(&channel->write_ready);
    pthread_mutex_unlock(&channel->lock);
    return result;
}

void _poison_int(struct _int_channel *channel) {
    pthread_mutex_lock(&channel->lock);
    channel->poisoned = true;
    pthread_cond_signal(&channel->read_ready);
    pthread_mutex_unlock(&channel->lock);
}

void _poison_char(struct _char_channel *channel) {
    pthread_mutex_lock(&channel->lock);
    channel->poisoned = true;
    pthread_cond_signal(&channel->read_ready);
    pthread_mutex_unlock(&channel->lock);
}

bool _wait_for_more_int(struct _int_channel *channel) {
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

bool _wait_for_more_char(struct _char_channel *channel) {
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

}\n"
