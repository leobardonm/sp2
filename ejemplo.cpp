#include <iostream>
#include "miarchivo.h"

using namespace std;

// Comentario de línea
/*
   Comentario multilínea
   que abarca varias líneas
*/

#define PI 3.14159

int main() {
    int entero = 42;
    float real = -3.14;
    bool bandera = true;
    char letra = 'A';
    const char* mensaje = "Hola, mundo!";
    
    if (entero > 0 && bandera) {
        cout << mensaje << endl;
    } else {
        cout << "Valor no válido" << endl;
    }

    for (int i = 0; i < 5; ++i) {
        cout << i << " ";
    }

    return 0;
}
