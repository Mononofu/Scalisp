package Scalisp

import java.io.FileOutputStream;
import java.io.PrintStream;

import org.objectweb.asm.ClassWriter;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.objectweb.asm.Type;
import org.objectweb.asm.commons.GeneratorAdapter;
import org.objectweb.asm.commons.Method;

object Compiler {
  def compile(src: String) = {
    val h = new Hello
    h.generate
  }
}

class Hello extends ClassLoader with Opcodes {

        def generate() {

        import Opcodes._

        val cw = new ClassWriter(ClassWriter.COMPUTE_MAXS)
        cw.visit(V1_1, ACC_PUBLIC, "Example", null, "java/lang/Object", null);

        // creates a GeneratorAdapter for the (implicit) constructor
        var m = Method.getMethod("void <init> ()");
        var mg = new GeneratorAdapter(ACC_PUBLIC,
                m,
                null,
                null,
                cw);
        mg.loadThis();
        mg.invokeConstructor(Type.getType(classOf[Object]), m);
        mg.returnValue();
        mg.endMethod();

        m = Method.getMethod("int calc ()");
        mg = new GeneratorAdapter(ACC_PUBLIC + ACC_STATIC, m, null, null, cw);
        mg.push(5);
        mg.returnValue();
        mg.endMethod();

        // creates a GeneratorAdapter for the 'main' method
        m = Method.getMethod("void main (String[])");
        mg = new GeneratorAdapter(ACC_PUBLIC + ACC_STATIC, m, null, null, cw);
        mg.getStatic(Type.getType(classOf[System]),
                "out",
                Type.getType(classOf[PrintStream]));
        //mg.push("Hello world! ");
        mg.invokeStatic(Type.getType("Example"), Method.getMethod("int calc()"));
        mg.invokeVirtual(Type.getType(classOf[PrintStream]),
                Method.getMethod("void println (int)"));
        mg.returnValue();
        mg.endMethod();

        cw.visitEnd();

        val code = cw.toByteArray();
        val loader = new Hello();
        val exampleClass = loader.defineClass("Example", code, 0, code.length);

        // uses the dynamically generated class to print 'Helloworld'
        exampleClass.getMethods()(0).invoke(null, Array(null):_*);
        }

}