#include <assert.h>
#include <pthread.h>
#include <stdio.h>

struct int_channel{
	int queue[100];
	int front;
	int back;   	// One past the last element
	int MAX_SIZE;
	int size;
	pthread_mutex_t lock;
	pthread_cond_t not_full;
	pthread_cond_t not_empty;
};

int init_int_channel(struct int_channel channel){
	if(pthread_mutex_init(&channel.lock, NULL) != 0){
		printf("Mutex init failed");
		return 1;
	}

	if(pthread_cond_init(&channel.not_full, NULL) +
	   pthread_cond_init(&channel.not_empty, NULL ) != 0){
		printf("Cond init failed");
		return 1;
	}
	channel.front = 0;
	channel.back = 0;
	return 0;
}

void enqueue_int(int element, struct int_channel* channel){
	pthread_mutex_lock(&channel->lock);
	while(channel->size >= channel->MAX_SIZE)
		pthread_cond_wait(&channel->not_full, &channel->lock);

	assert(channel->size < channel->MAX_SIZE);

	channel->queue[channel->back] = element;
	channel->back = (channel->back + 1) % channel->MAX_SIZE;

	channel->size++;
	pthread_cond_signal(&channel->not_empty);
	pthread_mutex_unlock(&channel->lock);
}

int dequeue_int(struct int_channel* channel){
	pthread_mutex_lock(&channel->lock);
	while(channel->size == 0)
		pthread_cond_wait(&channel->not_empty, &channel->lock);

	assert(channel->size > 0);

	int result = channel->queue[channel->front];
	channel->front = (channel->front + 1) % channel->MAX_SIZE;

	channel->size--;
	pthread_cond_signal(&channel->not_full);
	pthread_mutex_unlock(&channel->lock);
	return result;
}
