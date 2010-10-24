(*
 * The bot type stands for the result of a function which does not return, as
 * in (int -> bot).  SML currently uses types such as (int -> 'a) for functions
 * that do not return, which is fine except the let-polymorphism restriction
 * means you cannot use this as a first-class value.
 *
 * The function callcc in the Bot structure represents continuations as
 * functions of type ('a -> bot) rather than the opaque type ('a cont).
 *
 * There are two ways to call a function of type ('a -> bot).  First, you can
 * call it directly and then use upcast on the result.  Secondly, you can
 * convert it back to a polymorphic return value using invoke and then call it.
 * Invoke also makes sure the function throws away the current context before
 * running, which prevents memory leaks.
 *)

signature BOT =
   sig
      type bot
      val upcast : bot -> 'a
      val callcc : (('a -> bot) -> 'a) -> 'a
      val invoke : ('a -> bot) -> 'a -> 'b
   end

structure Bot :> BOT =
   struct
      structure Cont = SMLofNJ.Cont
      type bot = unit
      exception DoesNotOccur
      fun upcast () = raise DoesNotOccur
      fun callcc f = Cont.callcc (fn cont => f (Cont.throw cont))
      fun invoke f = Cont.throw (Cont.isolate f)
   end
