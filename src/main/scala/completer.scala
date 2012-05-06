package Scalisp

import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.SortedSet;
import java.util.TreeSet;
import scala.tools.jline;



class StringsCompleter(strs: Iterable[String]) extends jline.console.completer.Completer {
    val strings = new TreeSet[String]()

    
    import scala.collection.JavaConverters._
    setStrings(strs)

    def setStrings(strs: Iterable[String]) {
        strings.clear()
        strings.addAll(strs.toList.asJava)
    }

    def getStrings() = strings

    def complete(buffer: String, cursor: Int, candidates: java.util.List[CharSequence]): Int = {

        val buf = buffer.substring(1+math.max(buffer.lastIndexOf("("),
                                                  buffer.lastIndexOf(" ")))
        
        if (buffer == null) {
            candidates.addAll(strings);
        }
        else {
            
            for (m <- strings.tailSet(buf).asScala) {

                if (!m.startsWith(buf)) {
                    return cursor - buf.length
                }
                candidates.add(m);
            }
        }

        if (candidates.size() == 1) {
            candidates.set(0, candidates.get(0) + " ");
        }

        cursor - buf.length
    }
}