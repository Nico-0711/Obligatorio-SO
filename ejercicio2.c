#define _XOPEN_SOURCE 700
#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>
#include <semaphore.h>
#include <unistd.h>
#include <time.h>

#define TOTAL_PASAJEROS 100
#define TOTAL_OFICINISTAS 5
#define LECTURAS_POR_PASAJERO 3
#define CAMBIOS_POR_OFICINISTA 3

// Demoras máximas según consigna
#define MAX_DELAY_PASAJERO 3   
#define MAX_DELAY_OFICINISTA 5 

// Estado compartido
int versionCartel = 0;

// Semáforos
sem_t mutexLectores;     // protege cantidadLectores
sem_t bloqueoEscritura;  // asegura exclusión mutua para escribir
sem_t colaGeneral;       // evita inanición de escritores


int cantidadLectores = 0;

void *comportamientoPasajero(void *arg) {
    int id = *(int *)arg;
    free(arg);

    for (int i = 0; i < LECTURAS_POR_PASAJERO; i++) {

        // Espera simulada 
        sleep(rand() % (MAX_DELAY_PASAJERO + 1));

        // Entrada a la región de lectores
        sem_wait(&colaGeneral);
        sem_wait(&mutexLectores);

        cantidadLectores++;
        if (cantidadLectores == 1)
            sem_wait(&bloqueoEscritura);  // primer lector bloquea escritores

        sem_post(&mutexLectores);
        sem_post(&colaGeneral);

        // Lectura del cartel
        printf("Pasajero %d está mirando el cartel (versión %d)\n", id, versionCartel);
        fflush(stdout);

        sleep(rand() % (MAX_DELAY_PASAJERO + 1));

        // Salida de lector
        sem_wait(&mutexLectores);

        cantidadLectores--;
        if (cantidadLectores == 0)
            sem_post(&bloqueoEscritura);  // último lector libera escritores

        sem_post(&mutexLectores);
    }

    return NULL;
}

void *comportamientoOficinista(void *arg) {
    int id = *(int *)arg;
    free(arg);

    for (int i = 0; i < CAMBIOS_POR_OFICINISTA; i++) {

        // Espera aleatoria antes de hacer una modificación
        sleep(rand() % (MAX_DELAY_OFICINISTA + 1));

        // Entrada del escritor
        sem_wait(&colaGeneral);      // orden justo
        sem_wait(&bloqueoEscritura); // exclusión mutua

        // Escritura
        printf("Oficinista %d está modificando el cartel\n", id);
        fflush(stdout);

        sleep(rand() % (MAX_DELAY_OFICINISTA + 1));

        versionCartel++; // actualización real del recurso

        // Salida del escritor
        sem_post(&bloqueoEscritura);
        sem_post(&colaGeneral);
    }

    return NULL;
}

int main(void) {
    srand(time(NULL));

    sem_init(&mutexLectores, 0, 1);
    sem_init(&bloqueoEscritura, 0, 1);
    sem_init(&colaGeneral, 0, 1);

    pthread_t pasajeros[TOTAL_PASAJEROS];
    pthread_t oficinistas[TOTAL_OFICINISTAS];

    printf("=== Simulación del Cartel del Aeropuerto ===\n\n");

    // Crear oficinistas (escritores)
    for (int i = 0; i < TOTAL_OFICINISTAS; i++) {
        int *id = malloc(sizeof(int));
        *id = i + 1;
        pthread_create(&oficinistas[i], NULL, comportamientoOficinista, id);
    }

    // Crear pasajeros (lectores)
    for (int i = 0; i < TOTAL_PASAJEROS; i++) {
        int *id = malloc(sizeof(int));
        *id = i + 1;
        pthread_create(&pasajeros[i], NULL, comportamientoPasajero, id);
    }

    // Esperar todos los hilos
    for (int i = 0; i < TOTAL_OFICINISTAS; i++)
        pthread_join(oficinistas[i], NULL);

    for (int i = 0; i < TOTAL_PASAJEROS; i++)
        pthread_join(pasajeros[i], NULL);

    sem_destroy(&mutexLectores);
    sem_destroy(&bloqueoEscritura);
    sem_destroy(&colaGeneral);

    printf("\n=== Fin de la Simulación ===\n");
    printf("Versión final del cartel: %d\n", versionCartel);
    printf("Cambios esperados: %d\n", TOTAL_OFICINISTAS * CAMBIOS_POR_OFICINISTA);

    return 0;
}
