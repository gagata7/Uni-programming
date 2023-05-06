//Agata Pokorsks
//Lista 3 zadanie 1
//to jest biblioteka do zadania

using System;

public class Lista<T>{
  T val;
  Lista<T> next;
  public bool is_empty(){
    if(this.next == null) return true;
    return false;
  }

  public void push_back(T elem){
    while(this.next != null){this.next = this.next.next;}
    this.next = new Lista<T>();
    this.next.val = elem;
  }

  public T pop_back(){
    if(is_empty()){
      Console.WriteLine("Nie mozna usunac czegos z poczatku jesli kolejka pusta!");}
    if(this.next.next == null){
      T pom = this.next.val;
      this.next = null;
      return pom;
    }
    return this.next.pop_back();
  }

  public void push_front(T elem){
    Lista<T> head = new Lista<T>();
    head.val = elem;
    head.next = this;
  }

  public T pop_front(){
    if(is_empty()){
      Console.WriteLine("Nie mozna usunac czegos z poczatku jesli kolejka pusta!");}
    T pom = this.next.val;
    this.next = this.next.next;
    return pom;
  }

  public void print(){
    Console.WriteLine(this.val);
    if (this.next == null) return;
    this.next.print();
  }
}
