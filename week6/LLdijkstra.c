#include <stdlib.h>
#include <stdio.h>
#include <assert.h>

struct Node {
				double tt;
				int x, y, z;
				struct Node *next;
};

/* return head node */
void Head(struct Node** LLheadRef, struct Node* node) {
   node->tt = LLheadRef[0]->tt;
   node->x = LLheadRef[0]->x;
   node->y = LLheadRef[0]->y;
   node->z = LLheadRef[0]->z;
   node->next = LLheadRef[0]->next;

   printf("Head: (%d,%d,%d)\n", node->x, node->y, node->z);

}

/* push a node/vertex onto the head of the linked list */
void Push(struct Node** LLheadRef, double tt, int i, int j, int k) {
				struct Node *newNode = (struct Node*)malloc(sizeof(struct Node));
				
				newNode->tt= tt;
				newNode->x = i;
				newNode->y = j;
				newNode->z = k;
				newNode->next = *LLheadRef;

				*LLheadRef = newNode; 

                                printf(" PUSH   : (%d,%d,%d) == %f\n", i, j, k, (float)tt);
}

void removeNode(struct Node** LLheadRef, int index) {

				struct Node* curr = *LLheadRef;
				struct Node* prev = *LLheadRef;

				// need to remove the node #index pointers down/offset from head node
				int i = 0;
				while( i < index ) {
								prev = curr;
								curr = curr->next;
								i++;
				}

				//printf("removed the node at index %i with tt value %f\n",i,curr->tt);
				//bridge the "next" pointer between the around the nodes of curr
				if( index == 0 ) {
								*LLheadRef = curr->next;
								free(curr);
				}
				else if( curr->next != NULL) {
								prev->next = curr->next;
								free(curr);
				}
				else {
								prev->next = NULL;
								free(curr);
				}

}

/* update the travel time for a node at location i,j,k */
void updateNodeTT(struct Node** LLheadRef, double newTT, int i, int j, int k) {
				struct Node* curr = *LLheadRef;
				while( curr->next != NULL) {
								if( (curr->x == i) && (curr->y == j) && (curr->z==k) ) {
												curr->tt = newTT;
												return;
								}
								curr = curr->next;
				}
}

/* return the index of the minimum */
int FindMin(struct Node* LLhead) {
				int idx = -1;
				double min = 1000000.0;
				double newMin;
				struct Node* curr = LLhead;
				
				while(curr != NULL) {
								newMin = curr->tt;
								if(newMin < min) {
											min = newMin;
											idx++;
								}
								curr = curr->next;
				}
				//printf("min:%f\n",min);
				return idx;
}
				

/* delete the node with the minimum travel time */
void deleteMinNode(struct Node** LLheadRef, double* tt, int* i, int* j, int* k, int* length) {
    int idx = -1;
    int len =  0;
    double min = 99e99;
    double newMin;
    struct Node* curr = LLheadRef[0];
    struct Node* minNode = NULL;
   
    while(curr != NULL) {
        len += 1;
        newMin = curr->tt;
        if(newMin < min) {
            min = newMin;
            minNode = curr;
            idx++;
        }
        curr = curr->next;
    }
    removeNode(LLheadRef, idx);

    *tt = minNode->tt;
    *i  = minNode->x;
    *j  = minNode->y;
    *k  = minNode->z;
    *length = len - 1;

    printf(" DELETE : (%d,%d,%d) == %f, len==%d\n", *i, *j, *k, (float)*tt, *length);
}
				

int getLength(struct Node* LLhead) {
				int count = 0;
				struct Node* curr = LLhead;

				//printf("\ngetLength()\n");
				while( curr != NULL ) {
								count ++;
								printf("{ %.2f }->",curr->tt);
								curr = curr->next;
				}
				printf("\n");
				printf("size=%i\n",count);
				return count;
}


#ifdef USE_MAIN
int main() {
				double min;
				struct Node* head = NULL;

  			Push(&head, 50,0,0,0);
				Push(&head, 40,0,0,1);
				Push(&head, 11,0,1,1);
				Push(&head, 50,1,0,1);
				Push(&head, 5,1,1,0);
				Push(&head, 75,1,0,0);
				Push(&head, 99,1,1,1);
				Push(&head, 1,1,1,2);

				updateNodeTT(&head, 10000,1,1,1);

				while(getLength(head) != 0) {
								min = FindMin(head);
								//printf("idx of min:%f\n",min);
								removeNode(&head, min);
				}
				return 0;
}
#endif
