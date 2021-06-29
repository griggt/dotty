package dotty.tools.repl

import scala.reflect.{ClassTag, classTag}
import scala.util.control.Exception.catching

object ClassLoaderOps:
  extension (cl: ClassLoader)
    /** Create an instance with ctor args, or invoke errorFn before throwing. */
    def createInstance[T <: AnyRef : ClassTag](path: String, errorFn: String => Unit)(args: AnyRef*): T =
      def fail(msg: String) = error(msg, new IllegalArgumentException(msg))
      def error(msg: String, e: Throwable) = { errorFn(msg) ; throw e }
      try
        val clazz = Class.forName(path, /*initialize =*/ true, /*loader =*/ cl)
        if classTag[T].runtimeClass.isAssignableFrom(clazz) then
          val ctor =
            val maybes = clazz.getConstructors.filter(c =>
              c.getParameterCount == args.size
              && (c.getParameterTypes zip args).forall { case (k, a) => k.isAssignableFrom(a.getClass) })
            if maybes.size == 1 then maybes.head
            else fail(s"Constructor must accept arg list (${args.map(_.getClass.getName).mkString(", ")}): ${path}")
          (ctor.newInstance(args*)).asInstanceOf[T]
        else
          // TODO show is undefined; in the original code, it is imported from
          //  import scala.reflect.runtime.ReflectionUtils.show
          //errorFn(s"""Loader for ${classTag[T]}:   [${show(classTag[T].runtimeClass.getClassLoader)}]
          //           |Loader for ${clazz.getName}: [${show(clazz.getClassLoader)}]""".stripMargin)
          fail(s"Not a ${classTag[T]}: ${path}")
      catch
        case e: ClassNotFoundException =>
          error(s"Class not found: ${path}", e)
        case e @ (_: LinkageError | _: ReflectiveOperationException) =>
          error(s"Unable to create instance: ${path}: ${e.toString}", e)

    /** Load and link a class with this classloader */
    def tryToLoadClass[T <: AnyRef](path: String): Option[Class[T]] =
      tryClass(path, initialize = false)

    /** Load, link and initialize a class with this classloader */
    def tryToInitializeClass[T <: AnyRef](path: String): Option[Class[T]] =
      tryClass(path, initialize = true)

    private def tryClass[T <: AnyRef](path: String, initialize: Boolean): Option[Class[T]] =
      catching(classOf[ClassNotFoundException], classOf[SecurityException]) opt
        Class.forName(path, initialize, cl).asInstanceOf[Class[T]]

    /** The actual bytes for a class file, or an empty array if it can't be found. */
    def classBytes(className: String): Array[Byte] = classAsStream(className) match
      case null   => Array()
      case stream => dotty.tools.io.Streamable.bytes(stream)

    private inline def classAsStream(className: String) = cl.getResourceAsStream {
      if className.endsWith(".class") then className
      else s"${className.replace('.', '/')}.class"  // classNameToPath
    }
end ClassLoaderOps
