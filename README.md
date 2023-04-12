# Emerging2223Project
Emerging Programming Languages 20223/2023 Project Specification
(in Italian Only). Version 1.1.

## CHANGELOG

- Version 1.1
-- {park, PID, Ref} cambiato in {park, PID, X, Y, Ref} closes Issue #1.
   L'informazione poteva essere ricavata dall'ambient memorizzandola a partire dal precedente messaggio che verificava se il posteggio fosse vuoto o meno. Tuttavia, non essendoci un Ref condiviso, poteva essere ritornata un'informazione non correlata nel caso di perdita di messaggi.

## Descrizione del problema

Considerate una scacchiera toroidale le cui celle abbiano coordinate `{x,y}`.

Su questa scacchiera si muovono delle automobili che sono in grado di
comunicare fra di loro via message-passing.

Ogni cella della scacchiera rappresenta al tempo stesso un frammento di strada
e un posteggio. L'automobile può osservare la cella per capire se il posteggio
è libero o occupato e può propagare questa informazione verso le altre
automobili, utilizzando un algoritmo di gossiping.

Il ciclo di un'automobile consiste nello scegliere un parcheggio obiettivo che ritiene libero e navigare verso tale posteggio, osservando lo stato delle celle che attraversa. Giunta al posteggio obiettivo l'automobile rimane parcheggiata qualche tempo prima di scegliere un nuovo obiettivo e ripartire.

Nel caso in cui l'automobile riceva l'informazione che il posteggio obiettivo è ora occupato, cambia posteggio obiettivo.

Le automobili vengono casualmente creare e distrutte a intervalli regolari.

## Il sistema di attori

Il sistema di attori considerato prevede:
1. Un attore ambiente omniscente che rappresenta lo stato reale del mondo. In particolare l'ambiente conosce per ogni cella/posteggio il suo stato (libero o occupato). L'atomo `ambient` è registrato come PID dell'attore.
2. Un attore "wellknown" che viene contattato dalle automobili che entrano nel sistema di attori per ottenere i PID di altre automobili amiche da contattare nell'implementazione dell'algoritmo di gossiping. L'atomo `wellkown` è registrato come PID dell'attore.
3. Ogni automobile è rappresentata dal seguente sotto-sistema di attori:
   1. Un attore "main" che lancia gli altri attori ed è responsabile di ri-crearli nel caso di fallimento di uno di loro
   2. Un attore "friendship" che si preoccupa di mantenere 5 attori nella lista di attori, integrandone di nuovi nel caso in cui il numero scenda.
   3. Un attore "state" che si preoccupa di mantenere il modello interno dell'ambiente e le coordinate del posteggio obiettivo. In pratica l'attore registra per ogni cella l'ultima informazione giunta in suo possesso (posteggio libero/occupato/nessuna informazione) e propaga le nuove informazione ottenute agli amici (protocollo di gossiping). Inoltre cambia il posteggio obiettivo quando necessario (es. quando scopre che il posteggio è ora occupato).
   4. Un attore "detect" che si occupa di muovere l'automobile sulla scacchiera, interagendo con l'attore "ambient" per fare sensing dello stato di occupazione dei posteggi.
4. Un attore "render" che permette il debugging raccogliendo informazioni dagli altri attori per poterle visualizzare.

Il modello dei fallimenti implementato è il seguente:
- tutti gli attori che costituiscono un'automobile sono linkati fra loro e vengono fatti ripartire dall'attore "main" nel caso di crash
- le automobili sono linkate all'attore "ambient" e crashano nel caso di crash di quest'ultimo
- le automobili monitorano i propri amici per eliminarli dalla lista di amici nel caso di crash di questi

Ognuno degli attori che costituiscono un'automobile può comunicare con gli altri per mezzo di protoccoli privati. Inoltre ognuno degli attori nel sistema di attori partecipa ai protocolli pubblici descritti nella sezione seguente.

## Protocolli pubblici

### Protocollo per la Friendship

Il protocollo permette di chiedere agli amici la lista dei loro amici per poi
farne l'unione e scegliere da tale insieme i 5 attori da usare come amici. Viene implementato dagli attori "friendship" e, per quanto riguarda la sola risposta, dall'attore speciale `wellKnown`.

- `{getFriends, PID1, PID2, Ref}` inviato da un attore "friendship" (il cui PID è `PID1`) di un'automobile all'attore "friendship" di un'altra automobile. `PID2` è il PID dell'attore "state" dell'automobile mittente. `Ref` è una nuova reference che identifica la richiesta
- `{myFriends, PIDLIST, Ref}` è la risposta al messaggio precedente, inviata al PID `PID1` contenuto nel messaggio di richiesta. `Ref` è la reference ricevuta nella richiesta. `PIDLIST` è la lista di PID degli amici, intesa come PID degli attori "state".

Per inizializzare la lista di amici o qual'ora gli amici degli amici non siano sufficienti a ripristinare l'insieme di 5 amici, la richiesta `getFriends` viene inviata all'attore `wellKnown`.

A seguito della ricezione di un messaggio `getFriends`, il ricevente può aggiungere alla sua lista di amici il PID `PID2` contenuto nel messaggio, sempre con l'obiettivo di mantenere una lista di 5 amici.

### Protocolli per la Detection e il Parcheggio

L'attore "detect" di un'automobile sceglie un posteggio obiettivo libero interagendo con l'attore "state". Dopodichè, ogni 2s, si avvicina di una cella verso tale obiettivo. Se deve muoversi lungo entrambi gli assi (x e y), lo fa scegliendo randomicamente l'asse e muovendosi nella direzione che minimizza la distanza percorsa.

Dopo ogni movimento invia la richiesta
- `{isFree, PID, X, Y, Ref}` all'attore `ambient` dove `PID` è il PID dell'attore che ne fa richiesta e `Ref` una nuova reference
- `{status, Ref, IsFree}` è la risposta da parte dell'ambiente all'attore il cui PID `PID` era contenuto nella richiesta. Il booleano `IsFree` vale `true` sse il posteggio è libero.

In seguito alla ricezione del messaggio `status`, il messaggio viene condiviso con l'attore "state" tramite un protocollo privato.

Nel caso in cui sia stato raggiunto il posteggio obiettivo e questo sia libero:
- `{park, PID, X, Y, Ref}` viene invato all'attore "ambient" per dire che l'automobile sta parcheggiando. `Ref` è una nuova reference.
- `{leave, PID, Ref}` viene inviato dopo 1-5s (valore scelto casualmente) all'attore "ambient" per dire che l'automobile sta lasciando il posteggio. La reference contenuta nel messaggio deve essere identica a quella del messaggio precedente.

Nel caso in cui due automobili arrivino contemporanemante al posteggio e inviino entrambe un messaggio `park`, l'ambiente assegnerà il posteggio a quella arrivata per prima, killando la seconda automobile.

Durante il parcheggio, l'attore `ambient` monitora l'automobile parcheggiata in modo da liberare il posteggio qualora l'attore automobile venga killato.

Tramite un protocollo privato l'attore "detect" viene informato dall'attore "state" quando il parcheggio obiettivo divienta noto essere occupato, al fine di cambiare posteggio obiettivo scegliendone uno ritenuto libero.

### Protocollo di Gossiping

L'attore "state" mantiene il modello del mondo, ricevendo update sia dall'attore "detect" (tramite messaggi `status`), sia dagli altri attori "state", via messaggi `notifyStatus` descritti fra poco. Quando l'update comporta una modifica del modello interno (es. un posteggio che si riteneva essere occupato ora diventa libero, o viceversa), tale cambiamento viene notificato a tutti gli amici tramite messaggi `notifyStatus`:
- `{notifyStatus, X, Y, IsFree}`

Un protocollo privato permette all'attore "state" di ottenere la lista di amici correnti dall'attore "friendship". Il protocollo può essere ottimizzato per trasferire la lista solamente al cambiamento di questa.

## Il Main del progetto

Il progetto esporterà una funzione pubblica `main/0` la quale, una volta invocata:
1. crea l'attore "ambient" che si registra usando l'atomo `ambient`
2. crea l'attore "wellKnown" che si registra usando l'atomo `wellKnown`
3. crea l'attore "render" che si registra usando l'atomo `render`
4. crea un numero congruo di automobili assegnando a ognuna una posizione iniziale casuale e ricordandosi l'insieme dei PID dei loro attori "main"
5. a intervalli regolari sceglie casualmente delle automobili e le killa, rimpiazzandole con nuove automobili create in posizione casuale

## Debugging: attore `render`

Per permettere il debugging, dovrete implementare un ultimo attore "render",
il cui PID viene registrato associandolo all'atomo `render` e che si occupa di rappresentare lo stato del sistema di attori.

In particolare il "render" visualizza la griglia, indicando la posizione di ogni automobile, il suo stato (parcheggiata o meno), quale sia il posteggio obiettivo di ogni automobile e la lista di amici di ogni automobile. Queste informazioni vanno comunicate all'attore `render` dagli altri attori tramite i seguenti messaggi
- `{position, PID, X, Y}` la posizione dell'automobile, inviato dall'attore "detect"
- `{target, PID, X, Y}` la posizione del posteggio obiettivo dell'automobile, inviato dall'attore "detect"
- `{parked, PID, X, Y, IsParked}` inviata dall'attore "ambient" quando l'auto parcheggia/riparte
- `{friends, PID, PIDLIST}` inviata dall'attore "friendship" quando cambia la lista di amici

In tutti i messaggi qui sopra si deve utilizzare lo stesso PID per identificare le automobili (es. il PID dell'attore "state" o quello dell'attore "main").

La resa delle informazioni è liberamente implementabile a vostro gusto. Per esempio, potrebbe essere un'"immagine" in ASCII art seguita da una leggenda, rigenerata e mostrata a intervalli regolari.
