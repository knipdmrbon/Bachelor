# Bachelor

Hallo Dominik,

ich habe im Code einen Abschnitt von dir angepasst. Bei der Transformation der Daten (Zeile 135) möchtest du nur jene Werte log-transformieren die ungleich 0 sind. Bin dabei auf 2 Dinge gestoßen:

   1) die if-Abfrage beezieht sich nur auf den ersten Wert des Vektors x, da if nicht "vektorweise" funktioniert.
   2) Ich glaube eine Transformation sollte nur "ganzheitlich" auf die Daten angewendet werden. Also entweder wir transformieren eine Spalte gesamt oder gar nicht. Nur alle Werte != 0 zu transformieren ist glaube ich nicht konsistent.
   
   Damit man bei einer log-Transformation von 0 kein -Infinity bekommt, kann man folgende Dinge machen
        1) Addiere zu jedem Wert von x die Zahl 1 und transformiere dann --> log(x+1)
        2) Addiere zu jedem Wert von x den kleinsten positiven Wert aus dem Vektor x und transformiere dann --> log(x + min(x))
           Bei uns ist min(x) immer 0, daher habe ich 0.00000001 gewählt.
        
Ich habe gelesen, dass es grundsätzlich schwer/unmöglich ist eine Verteilung wie sie wir haben (viele 0er und dann noch ein paar andere Werte) auf eine Normalverteilung zu transformieren. Da die 0er bei uns so stark vertreten sind (spike) wird man in jeder transformierten Verteilung wieder einen spike haben --> nicht normalverteilt. 

Was habe ich gemacht:
   1) if Abfrage in Zeile 136 gelöscht
   2) Log-transformiert mit log(x + 0.00000001)
   
Alternative: 
   Lässt man alle 0-Werte weg und macht dann eine Log-Transformation dann hätte man normalverteilte Variablen. Können wir das für die Analyse argumentieren?
   
 
