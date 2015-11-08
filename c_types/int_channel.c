#include <assert.h>
#include <pthread.h>
#include <stdio.h>

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
