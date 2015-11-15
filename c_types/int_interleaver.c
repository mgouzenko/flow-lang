#include <assert.h>
#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>

struct int_channel{
    int queue[100];
    int front;
    int back;   	// One past the last element
    int MAX_SIZE;
    int size;
    bool poisoned;
    pthread_mutex_t lock;
    pthread_cond_t write_ready;
    pthread_cond_t read_ready;
};

int init_int_channel(struct int_channel *channel){
    if(pthread_mutex_init(&channel->lock, NULL) != 0){
        printf("Mutex init failed");
        return 1;
    }

    if(pthread_cond_init(&channel->write_ready, NULL) +
            pthread_cond_init(&channel->read_ready, NULL ) != 0){
        printf("Cond init failed");
        return 1;
    }
    channel->MAX_SIZE = 100;
    channel->front = 0;
    channel->back = 0;
    channel->poisoned = false;
    return 0;
}

void enqueue_int(int element, struct int_channel *channel){
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

int dequeue_int(struct int_channel *channel){
    pthread_mutex_lock(&channel->lock);
    assert(channel->size != 0);

    int result = channel->queue[channel->front];
    channel->front = (channel->front + 1) % channel->MAX_SIZE;

    channel->size--;
    pthread_cond_signal(&channel->write_ready);
    pthread_mutex_unlock(&channel->lock);
    return result;
}

void poison(struct int_channel *channel) {
    pthread_mutex_lock(&channel->lock);
    channel->poisoned = true;
    pthread_cond_signal(&channel->read_ready);
    pthread_mutex_unlock(&channel->lock);
}

bool wait_for_more(struct int_channel *channel) {
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


// token generator process args
struct _token_gen_for_array_size_five_args {
    struct int_channel *ochan;
    int *input;
};

// token generator process
void *token_gen_for_array_size_five(void *_args) {
    // process args
    struct int_channel *ochan = ((struct _token_gen_for_array_size_five_args *) _args)->ochan;
    int *input = ((struct _token_gen_for_array_size_five_args *) _args)->input;

    // token generator logic
    for (int i = 0; i < 5; i++) {
        enqueue_int(input[i], ochan);
    }
    poison(ochan);
    return 0;
}

// print_int process args
struct _print_int_args {
    struct int_channel *chan;
};

// print_int process
void *print_int(void *_args) {
    // process args
    struct int_channel *chan = ((struct _print_int_args *) _args)->chan;

    // print_int logic
    while (wait_for_more(chan)) {
        printf("%d\n", dequeue_int(chan));
    }
    return 0;
}

// interleaver process args
struct _interleaver_args {
    struct int_channel *chan1;
    struct int_channel *chan2;
    struct int_channel *ochan;
};

// interleaver process
void *interleaver(void *_args) {
    // process args
    struct int_channel *chan1 = ((struct _interleaver_args *) _args)->chan1;
    struct int_channel *chan2 = ((struct _interleaver_args *) _args)->chan2;
    struct int_channel *ochan = ((struct _interleaver_args *) _args)->ochan;

    // interleaver logic
    while(wait_for_more(chan1) || wait_for_more(chan2)) {
        if (wait_for_more(chan1)) enqueue_int(dequeue_int(chan1), ochan);
        if (wait_for_more(chan2)) enqueue_int(dequeue_int(chan2), ochan);
    }
    poison(ochan);
    return 0;
}

struct pthread_node{
	pthread_t thread;
	struct pthread_node *next;
};

struct pthread_node* head = NULL;

pthread_mutex_t thread_list_lock;

pthread_t* make_pthread_t(){
	pthread_mutex_lock(&thread_list_lock);
	struct pthread_node *new_pthread = (struct pthread_node *) malloc(sizeof(struct pthread_node));
	new_pthread->next = head;
	head = new_pthread;
	pthread_mutex_unlock(&thread_list_lock);
	return &(new_pthread->thread);
}

void wait_for_finish(){
	struct pthread_node* curr = head;
	while(curr){
		pthread_join(curr->thread, NULL);
		curr = curr->next;
	}
}

int main() {
	pthread_mutex_init(&thread_list_lock, NULL);

    struct int_channel *chan1 = (struct int_channel *) malloc(sizeof(struct int_channel));
    struct int_channel *chan2 = (struct int_channel *) malloc(sizeof(struct int_channel));
    struct int_channel *ochan = (struct int_channel *) malloc(sizeof(struct int_channel));
    init_int_channel(chan1);
    init_int_channel(chan2);
    init_int_channel(ochan);

    int array1[5] = {1, 2, 3, 4, 5};
    int array2[5] = {0, 10, 0, 10, 0};

    {
        pthread_t* _t = make_pthread_t();
        struct _token_gen_for_array_size_five_args _args = {
            chan1,
            array1,
        };
        pthread_create(_t, NULL, token_gen_for_array_size_five, (void *) &_args);
    };
    
    {
    	pthread_t* _t = make_pthread_t();
        struct _token_gen_for_array_size_five_args _args = {
            chan2,
            array2,
        };
        pthread_create(_t, NULL, token_gen_for_array_size_five, (void *) &_args);
    };

    {
        pthread_t* interleaver_t = make_pthread_t();
        struct _interleaver_args interleaver_t_args = {
            chan1,
            chan2,
            ochan,
        };
        pthread_create(interleaver_t, NULL, interleaver, (void *) &interleaver_t_args);
    };

    {
        pthread_t* print_int_t = make_pthread_t();
        struct _print_int_args print_int_t_args = {
            ochan,
        };
        pthread_create(print_int_t, NULL, print_int, (void *) &print_int_t_args);
    }

	wait_for_finish();
    return 0;
}
