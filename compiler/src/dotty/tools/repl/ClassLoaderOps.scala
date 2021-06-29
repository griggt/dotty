package dotty.tools.repl

object ClassLoaderOps:
  extension (cl: ClassLoader)
    /** The actual bytes for a class file, or an empty array if it can't be found. */
    def classBytes(className: String): Array[Byte] = classAsStream(className) match
      case null   => Array()
      case stream => dotty.tools.io.Streamable.bytes(stream)

    private inline def classAsStream(className: String) = cl.getResourceAsStream {
      if className.endsWith(".class") then className
      else s"${className.replace('.', '/')}.class"  // classNameToPath
    }
end ClassLoaderOps
