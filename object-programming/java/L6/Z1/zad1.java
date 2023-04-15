/*
Agata Pokorska
Lista 6 Zadanie 1
openjdk version "19.0.2" 2023-01-17
OpenJDK Runtime Environment (build 19.0.2+7-Ubuntu-0ubuntu322.04)
OpenJDK 64-Bit Server VM (build 19.0.2+7-Ubuntu-0ubuntu322.04, mixed mode, sharing)
*/

import java.io.*;
import java.util.*;

class Lista<T> implements Serializable{
    Node head;
    class Node implements Serializable{//żeby klasa implementowala interfejs, to jej pola też muszą
        T value; Node next;
        Node(T elem){
            value = elem;
            next = null;
        }
    }
    public void add_element(T elem){
        Node new_node = new Node(elem);
        if(this.head == null){
            this.head = new_node;
            return;
        }
        Node current = this.head;
        while(current.next != null){
            current = current.next;
        }
        current.next = new_node;
    }
    public T get_first(){
        if(this.head == null) return null;
        return this.head.value;
    }
    @Override public String toString(){
        String to_print = "";
        Node current = this.head;
        while(current != null){
            to_print+= current.value.toString();
            if(current.next != null){
                to_print+= ",";
            }
            current = current.next;
        }
        return to_print;
    }
}


class zad1{
    public static void main(String[] args)
    throws IOException, ClassNotFoundException{
        Lista l = new Lista<String>();
        l.add_element("banknot");
        l.add_element("szmal");
        l.add_element("kasa");
        l.add_element("zielone");
        
        FileOutputStream fos = new FileOutputStream("plik_do_zapisu.txt");
        ObjectOutputStream oos = new ObjectOutputStream(fos);
        oos.writeObject(l);
        oos.close();
        
        FileInputStream fis = new FileInputStream("plik_do_zapisu.txt");
        ObjectInputStream ois = new ObjectInputStream(fis);
        Lista l2 = (Lista)ois.readObject(); 
        ois.close();
        
        System.out.println(l2.toString());
    }
}
