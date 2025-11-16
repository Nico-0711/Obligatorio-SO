
/*
  lectores_escritores_aeropuerto.c
  Simulación POSIX del problema lectores-escritores para el panel del aeropuerto.

  Requisitos implementados:
  - Al menos 100 pasajeros (lectores) y 5 oficinistas (escritores).
  - Cada oficinista realiza al menos 3 modificaciones en el cartel.
  - Oficinistas: delay random <= 5s, Pasajeros: delay random <= 3s.
  - Mensajes por pantalla:
      "Pasajero X está mirando el cartel"
      "Oficinista X está modificando el cartel"

  Basado en el algoritmo clásico de lectores-escritores con semáforos (mutex / wrt).
  (Referencias del material entregado por el alumno sobre semáforos y lectores/escritores).
*/
#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>
#include <semaphore.h>
#include <unistd.h>
#include <string.h>
#include <time.h>
#include <stdatomic.h>

#define N_PASSENGERS 100
#define N_CLERKS 5
#define CHANGES_PER_CLERK 3

// Semáforos / mutex según el esquema del PDF: mutex para proteger rdr y wrt para escritores
sem_t mutex; // protege 'readers_count'
sem_t wrt;   // bloqueo para escritores (y para lectores la primera entrada)

int readers_count = 0; // número de lectores actuales (protegido por 'mutex')

// Panel compartido (cadena simple para simular estado)
char panel[256];

// contador de escritores pendientes para avisar a pasajeros cuando terminaron
atomic_int writers_left;

// función auxiliar: sleep aleatorio en milisegundos
void ms_sleep(long ms) {
    if (ms <= 0) return;
    usleep(ms * 1000);
}

long rand_range(long min_ms, long max_ms) {
    if (max_ms <= min_ms) return min_ms;
    return min_ms + rand() % (max_ms - min_ms + 1);
}

void *passenger_thread(void *arg) {
    int id = *((int*)arg);
    free(arg);

    // Los pasajeros continúan leyendo hasta que todos los oficinistas hayan terminado.
    while (atomic_load(&writers_left) > 0) {
        // Demora aleatoria hasta 3s antes de mirar
        long wait_ms = rand_range(0, 3000);
        ms_sleep(wait_ms);

        // Entrada lector
        sem_wait(&mutex);
        readers_count++;
        if (readers_count == 1) {
            // Primero lector bloquea escritores
            sem_wait(&wrt);
        }
        sem_post(&mutex);

        // Leer el panel (sección crítica de lectura concurrente)
        printf("Pasajero %d está mirando el cartel\n", id);
        // Simular tiempo de lectura corto (100-500 ms)
        ms_sleep(rand_range(100, 500));
        // Opcional: mostrar el estado actual del panel
        printf("  (Pasajero %d ve): %s\n", id, panel);

        // Salida lector
        sem_wait(&mutex);
        readers_count--;
        if (readers_count == 0) {
            // Si ya no hay lectores, liberar a escritores
            sem_post(&wrt);
        }
        sem_post(&mutex);
    }

    // Al terminar los escritores, cada pasajero puede hacer una última lectura final
    // para ver el estado final (opcional)
    // Entrada lector final
    sem_wait(&mutex);
    readers_count++;
    if (readers_count == 1) sem_wait(&wrt);
    sem_post(&mutex);

    printf("Pasajero %d está mirando el cartel (última lectura)\n", id);
    ms_sleep(rand_range(100, 300));
    printf("  (Pasajero %d ve): %s\n", id, panel);

    sem_wait(&mutex);
    readers_count--;
    if (readers_count == 0) sem_post(&wrt);
    sem_post(&mutex);

    return NULL;
}

void *clerk_thread(void *arg) {
    int id = *((int*)arg);
    free(arg);

    for (int i = 0; i < CHANGES_PER_CLERK; ++i) {
        // Delay aleatorio antes de modificar (<= 5s)
        long wait_ms = rand_range(0, 5000);
        ms_sleep(wait_ms);

        // Pedir acceso exclusivo para escribir
        sem_wait(&wrt);

        // Simulación de modificación del cartel
        printf("Oficinista %d está modificando el cartel\n", id);
        // Cambiamos el contenido del panel para mostrar la modificación
        time_t t = time(NULL);
        struct tm tm = *localtime(&t);
        char new_state[256];
        snprintf(new_state, sizeof(new_state),
                 "Oficinista %d actualizó a las %02d:%02d:%02d (cambio %d of %d)",
                 id, tm.tm_hour, tm.tm_min, tm.tm_sec, i+1, CHANGES_PER_CLERK);
        strncpy(panel, new_state, sizeof(panel)-1);
        panel[sizeof(panel)-1] = '\\0';

        // Simular tiempo de escritura (200-800 ms)
        ms_sleep(rand_range(200, 800));

        // Liberar el semáforo
        sem_post(&wrt);
    }

    // Una vez terminado, decrementamos el contador de oficinistas restantes
    atomic_fetch_sub(&writers_left, 1);
    return NULL;
}

int main(void) {
    srand(time(NULL));

    // Inicialización
    sem_init(&mutex, 0, 1);
    sem_init(&wrt, 0, 1);
    atomic_init(&writers_left, N_CLERKS);

    // Estado inicial del panel
    strncpy(panel, "Panel operativo: sin cambios aún", sizeof(panel)-1);
    panel[sizeof(panel)-1] = '\\0';

    pthread_t passengers[N_PASSENGERS];
    pthread_t clerks[N_CLERKS];

    // Crear threads de oficinistas (escritores)
    for (int i = 0; i < N_CLERKS; ++i) {
        int *id = malloc(sizeof(int));
        *id = i + 1;
        if (pthread_create(&clerks[i], NULL, clerk_thread, id) != 0) {
            perror("pthread_create clerk");
            exit(1);
        }
    }

    // Crear threads de pasajeros (lectores)
    for (int i = 0; i < N_PASSENGERS; ++i) {
        int *id = malloc(sizeof(int));
        *id = i + 1;
        if (pthread_create(&passengers[i], NULL, passenger_thread, id) != 0) {
            perror("pthread_create passenger");
            exit(1);
        }
    }

    // Esperar a que todos los oficinistas terminen
    for (int i = 0; i < N_CLERKS; ++i) {
        pthread_join(clerks[i], NULL);
    }

    // Esperar a que los pasajeros terminen (ya saldrán al detectar writers_left == 0)
    for (int i = 0; i < N_PASSENGERS; ++i) {
        pthread_join(passengers[i], NULL);
    }

    // Limpiar semáforos
    sem_destroy(&mutex);
    sem_destroy(&wrt);

    printf("Todos los oficinistas terminaron y todos los pasajeros han leído el estado final.\n");
    return 0;
}
