package js7.base.catsutils

import scala.reflect.ClassTag

object IArrayExtensions:

  extension [A](iarray: IArray[A])
    // Only for unchanged result type:
    //def mapOrKeep(pf: PartialFunction[A, A])/*(using ClassTag[A])*/: IArray[A] =
    //  given ClassTag[A] = ClassTag(iarray.getClass.asInstanceOf[Class[Array[A]]].getComponentType.asInstanceOf[Class[A]])
    //  iarray.map: a =>
    //    pf.applyOrElse(a, identity)

    def mapOrKeep[A1 >: A](pf: PartialFunction[A, A1])(using ClassTag[A1]): IArray[A1] =
      iarray.map: a =>
        pf.applyOrElse(a, identity)
