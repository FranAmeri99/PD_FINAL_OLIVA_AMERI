from curses import termattrs
import random as rd
import re

import os

def cls():
    os.system('cls' if os.name=='nt' else 'clear')

# now, to clear the screen

colores = [ 'NEGRO' , 'AZUL' , 'AMARILLO' , 'ROJO' , 'VERDE' ]
pozo = []
baraja=[]

def crearBaraja():
    baraja=[]
    pozo = []
    for i in range(0,9):
        for color in colores[1:]:
            if(color!='NEGRO'): #SALTEO EL NEGRO
                baraja.append({"color":color, "valor":str(i)} )
    
    for _ in range(4): #AGREGO CARTAS ESPECIALES
        baraja.append({"color":"NEGRO", "valor":"+4"})
        baraja.append({"color":"NEGRO", "valor":"COMODIN"})
    
    for _ in range(4): #AGREGO CARTAS ESPECIALES
        for color in colores[1:]:
            baraja.append({"color":color, "valor":"+2"})
            baraja.append({"color":color, "valor":"SALTAR JUGADOR"})
            baraja.append({"color":color, "valor":"CAMBIAR SANTIDO"})

    return baraja

def pintarCarta(carta):
    return(((carta["color"]+" ") if carta["color"]!="NEGRO" else " ") + carta["valor"])

def mostrarMano(jugador, numeradas = True):
    i = 1
    for carta in jugador["Mano"]:
        print((str(i) if numeradas else '') +") " + pintarCarta(carta))
        i+=1   

def cumpleLasReglas(cartaEscogida, cartaEnMesa):
    if cartaEscogida["color"] == "NEGRO":
        print("------------NEGRO---------------")
        return True
   
    if cartaEnMesa['valor'] == "SE SALTEO UN JUGADOR " and cartaEscogida["valor"] == "SALTAR JUGADOR":
        return True

    if cartaEnMesa["color"] == cartaEscogida["color"] or  cartaEnMesa["valor"] == cartaEscogida["valor"] :
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
    #cls()
    if cartaEnMesa["valor"]=="+2":
        print("Roba dos cartas")
        for _ in range(2):
            jugador, baraja = robarCarta(baraja, jugador)

    if cartaEnMesa["valor"]=="+4":
        print("Roba cuatro cartas")
        for _  in range(4):
            jugador, baraja = robarCarta(baraja, jugador)

    if cartaEnMesa["valor"]!="SALTAR JUGADOR":
        while repetir:
            cls()
            if baraja == None:
                print("No hay mas cartas en el mazo! \n")
            print( " - Turno de " + jugador["Nombre"] + " -")
            print("\n")
            print(len(baraja))
            print("Orden Turnos: ")
            orden = 1;
            for pepe in (jugadores):
                print("1) " + pepe["Nombre"])
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
                    if(cartaEscogida["color"]=="NEGRO"):
                        cartaEscogida["color"]= escogerColor()
                    if cartaEscogida["valor"] == "CAMBIAR SANTIDO":

                        jugadores = jugadores[::-1]

                    repetir = False
                else:        
                    print("Esa carta no vale")
    else:
        cartaEnMesa["valor"] = "SE SALTEO UN JUGADOR" 
        cartaEscogida = cartaEnMesa           
    return cartaEscogida, baraja, jugadores

def puntuar(carta):

   if carta["valor"]=="+4":
      return 100
   if carta["valor"]=="+2":
      return 20
   if carta["valor"]=="COMODIN":
      return 20
   if carta["valor"]=="SALTAR JUGADOR":
      return 50
   if carta["valor"]=="CAMBIAR SANTIDO":
      return 50
   return int(carta["valor"])

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
        if len(baraja) == 0:
            continuar = False
        
        for jugador in jugadores:
            cls()
            if pozo[-1]["color"] == "NEGRO":
                print("Cambia el Color:")
                pozo[-1]["color"] = escogerColor()
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
if __name__ == "__main__" : 
    main()
