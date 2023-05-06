/*
Agata Pokorska
Lista 5 Zadanie 1
openjdk version "19.0.2" 2023-01-17
OpenJDK Runtime Environment (build 19.0.2+7-Ubuntu-0ubuntu322.04)
OpenJDK 64-Bit Server VM (build 19.0.2+7-Ubuntu-0ubuntu322.04, mixed mode, sharing)
*/

//kolejność kolorów od najważniejszego: pik, kier, karo, trefl
class Pik implements Comparable<Pik>{
  @Override
  public int compareTo(Pik o){
    if(o instanceof Trefl) return 1;
    if(o instanceof Karo) return 1;
    if(o instanceof Kier) return 1;
    return 0;
  }
}

class Kier extends Pik{
  @Override
  public int compareTo(Pik o){
    if(o instanceof Trefl) return 1;
    if(o instanceof Karo) return 1;
    if(o instanceof Kier) return 0;
    return -1;
  }
}

class Karo extends Kier{
  @Override
  public int compareTo(Pik o){
    if(o instanceof Trefl) return 1;
    if(o instanceof Karo) return 0;
    if(o instanceof Kier) return -1;
    return -1;
  }
}

class Trefl extends Karo{
  @Override
  public int compareTo(Pik o){
    if(o instanceof Trefl) return 0;
    if(o instanceof Karo) return -1;
    if(o instanceof Kier) return -1;
    return -1;
  }
}

class OrderedList<T extends Comparable<T>> {
  Node head;
  class Node {
    T value; Node next; Node prev;
    Node(T elem){
      value = elem;
      next = null;
      prev = null;
    }
  }
  public void add_element(T elem){
    Node new_node = new Node(elem);
    if(this.head == null){
      this.head = new_node;
      return;
    }
    Node current = this.head;
    while(current.next != null && current.value.compareTo(elem) < 0)
    if(current.next == null){
      if(current.value.compareTo(elem) <= 0){
        current.next = new_node;
        new_node.prev = current;
      }
    }
    else{
      new_node.next = current;
      current.prev.next = new_node;
      new_node.prev = current.prev;
      current.prev = new_node;
    }
  }

  public T get_first(){
    return this.head.value;
  }

  @Override public String toString(){
    String to_print = "";
    Node current = this.head;
    while(current.next != null){
      to_print+= current.value.toString();
      current = current.next;
    }
    return to_print;
  }
}

class zad1{
  public static void main(String[] args) {
    OrderedList lista = new OrderedList<Pik>();
    lista.add_element(new Trefl());
    lista.add_element(new Pik());
    lista.add_element(new Kier());
    System.out.println(lista.get_first().toString());
  }
}
