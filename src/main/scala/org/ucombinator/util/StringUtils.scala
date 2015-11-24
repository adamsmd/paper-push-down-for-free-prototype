package org.ucombinator.util

import java.io.File

/**
 * @author Ilya Sergey
 */
object StringUtils {

  def truncateIfLong(s: String, l: Int): String = {
    if (s == null || l <= 0) s
    else if (s.length() <= l) s
    else s.take(l) + "..."
  }

  def trimFileName(filename: String) = {

    import File.separator

    def trimInternal(name: String) = {
      if (name == null || !name.contains(".") || name.startsWith(".")) {
        name
      } else {
        name.substring(0, name.indexOf("."))
      }
    }

    val nName = trimInternal(filename)

    if (nName == null || !nName.contains(separator) || nName.endsWith(separator)) {
      nName
    } else {
      nName.substring(nName.lastIndexOf(separator) + 1)
    }
  }

}
