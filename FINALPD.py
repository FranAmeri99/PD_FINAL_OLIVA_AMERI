from curses import termattrs
import random as rd
import re
import os
from collections import deque

def cls():
    os.system('cls' if os.name=='nt' else 'clear')

# now, to clear the screen

colores = [ 'NEGRO' , 'AZUL' , 'AMARILLO' , 'ROJO' , 'VERDE' ]
pozo = []
baraja=[]

def crearBaraja():
    baraja=[]
    coloresComunes = deque(colores)
    coloresComunes.popleft()
    baraja = [(n, p) for n in list(range(0,9)) for p in coloresComunes] * 2
    especiales = ['+4', 'COMODIN']
    colorEspecial = [colores[0]]
    barajaEspeciales = [(n, p) for n in especiales for p in colorEspecial] * 4  
    especialesMasDos = ['+2', 'SALTAR JUGADOR', 'CAMBIAR SENTIDO']
    barajaMasDos = [(n, p) for n in especialesMasDos for p in coloresComunes] * 2
    return baraja + barajaEspeciales + barajaMasDos

def pintarCarta(carta):
    return(((str(carta[1])+" ") if carta[1]!="NEGRO" else " ") + str(carta[0]))

def mostrarMano(jugador, numeradas = True):
    i = 1
    for carta in jugador["Mano"]:
        print((str(i) if numeradas else '') +") " + pintarCarta(carta))
        i+=1   

def cumpleLasReglas(cartaEscogida, cartaEnMesa):
    if cartaEscogida[1] == "NEGRO":
        print("------------NEGRO---------------")
        return True
   
    if cartaEnMesa[1] == "SE SALTEO UN JUGADOR " and cartaEscogida[1] == "SALTAR JUGADOR":
        return True

    if cartaEnMesa[1] == cartaEscogida[1] or  cartaEnMesa[1] == cartaEscogida[1] :
        return True
   
    else:
        return False

def escogerColor():
    repetir = True
    colorEscogido = ""
    while repetir:
        i = 1
        for color in colores[1:]:
            print(str(i)+") "+ color )
            i += 1
        colorEscogido = input("Escoga un color: ")
        if colorEscogido.isnumeric() and int(colorEscogido)>0 and int(colorEscogido) <= len(colores)-1:
            repetir = False
    return colores[int(colorEscogido)]

def robarCarta(baraja,jugador):
    if len(baraja) > 0:
        jugador["Mano"].append(baraja[0])
        baraja = baraja[1:]
    else:
        print("no hay mas cartas :(")
    return jugador, baraja 

def escogerCarta(jugador, cartaEnMesa, baraja, jugadores):
    
    repetir = True
    cls()
    if cartaEnMesa[0]=="+2":
        print("Roba dos cartas")
        for _ in range(2):
            jugador, baraja = robarCarta(baraja, jugador)

    if cartaEnMesa[0]=="+4":
        print("Roba cuatro cartas")
        for _  in range(4):
            jugador, baraja = robarCarta(baraja, jugador)

    if cartaEnMesa[0]!="SALTAR JUGADOR":
        while repetir:
            cls()
            if baraja == None:
                print("No hay mas cartas en el mazo! \n")
            print( " - Turno de " + jugador["Nombre"] + " -")
            print("\n")
            print("Cantidad de cartas en el mazo: " + str(len(baraja)) + "\n")
            print("Orden Turnos: ")
            for jugadorT in (jugadores):
                print("- " + jugadorT["Nombre"])
                # print(jugadorT["Mano"])
            print("\n")
            mostrarMano(jugador, True)

            print("\r \n \r \n Carta en la mesa:" + pintarCarta(pozo[-1]))

            idCartaEscogida = input("Que carta quieres tirar (R para robar):")
            
            if idCartaEscogida == "R" or idCartaEscogida == "r":
                if(len(baraja)>0):
                    jugador, baraja = robarCarta(baraja, jugador)
                else:
                    print("No hay mas cartas en el maso para robar")
            elif idCartaEscogida.isnumeric() and int(idCartaEscogida) > 0 and int(idCartaEscogida)<= len(jugador["Mano"]):
                cartaEscogida = jugador["Mano"][int(idCartaEscogida)-1]
                if cumpleLasReglas(cartaEscogida, cartaEnMesa):
                    jugador["Mano"] = jugador["Mano"][0: int(idCartaEscogida)-1] + jugador["Mano"][int(idCartaEscogida):] 
                    if(cartaEscogida[1]=="NEGRO"):
                        print(cartaEscogida)
                        # cartaEscogida[1]= escogerColor()
                    if cartaEscogida[1] == "CAMBIAR SANTIDO":

                        jugadores = jugadores[::-1]

                    repetir = False
                else:        
                    print("Esa carta no vale")
    else:
         
        cartaEscogida = ("SE SALTEO UN JUGADOR",cartaEnMesa[1])           
    return cartaEscogida, baraja, jugadores

def puntuar(carta):

   if carta[0]=="+4":
      return 100
   if carta[0]=="+2":
      return 20
   if carta[0]=="COMODIN":
      return 20
   if carta[0]=="SALTAR JUGADOR":
      return 50
   if carta[0]=="CAMBIAR SANTIDO":
      return 50
   return int(carta[0])

def calcularPuntos(jugador):
    contador = 0 
    for carta in jugador["Mano"]:
        contador += puntuar(carta)
    return contador

def inicio():
    cls()
    print(" ----- Bienvenido al Uno! ----- ")
    cantidad = 0
    while cantidad < 2 or cantidad > 4:
        cantidad = input("Indique la cantidad de jugadores (min 2 - max 4): ")
        cantidad = int(cantidad)
    return cantidad

def crearJugadores(cantidad):
    jugadores = list(dict())
    for x in range(cantidad):
        jugador = {'Nombre': input("Ingrese el nombre del jugador [ "+ str(x+1) +" ]: "), 'Mano': [], 'Tipo': "Humano"}
        jugadores.append(jugador)
    return jugadores

def main():
        
    baraja=crearBaraja()
    print(baraja)
    cantidad_jugadores = inicio()

    jugadores = crearJugadores(cantidad_jugadores)

    rd.shuffle(baraja)
    rd.shuffle(baraja)
    rd.shuffle(baraja)

    """Reparto cartar """
    for _ in range(7):
        for jugador in jugadores:
            jugador["Mano"].append(baraja[0])
            baraja = baraja[1:]

    

    pozo.append(baraja[0])
    baraja = baraja[1:]
    continuar = True
    while continuar:
        for jugador in jugadores:
            cls()
            print(baraja)
            if pozo[-1][1] == "NEGRO":
                print("Cambia el Color:")
                print(pozo[-1][1])
                nuevoCartaV = pozo[-1][0]
                nuevoCartaC = escogerColor()
                nuevaCarta =(nuevoCartaV,nuevoCartaC)
                pozo[-1] = nuevaCarta

            cartaEscogida, baraja, jugadores = escogerCarta(jugador, pozo[-1], baraja, jugadores)
            if cartaEscogida != None:
                pozo.append(cartaEscogida)
            if len(jugador["Mano"])==0:
                cls()
                print(" ----------- " + jugador["Nombre"] + " GANA LA PARTIDA ------")
                print("\n")
                for perdedor in jugadores:
                    if perdedor != jugador:
                        print(" - " + perdedor["Nombre"]+" perdio - Puntaje final: ("+ str(calcularPuntos(perdedor))+") pts")
                continuar = False
        if len(baraja) == 0:
            continuar = False
if __name__ == "__main__" : 
    main()
