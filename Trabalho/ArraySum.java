import java.util.Random;
import java.util.concurrent.atomic.AtomicInteger;

public class ArraySum {
  
 	 // Programa de teste.
	public static void main(String[] args) throws InterruptedException {
		int size = args.length >= 1 ? 
		Integer.parseInt(args[0]) : 10000;
		int numberOfThreads = args.length >= 2 ?
		Integer.parseInt(args[0]) : 2;

		int[] array = buildArray(size);
		
		long t;
		int sum;
		
		// Soma sequencial (sem multi-threading).
		t = System.currentTimeMillis();
		sum = sumArraySeq(array);
		t = System.currentTimeMillis() - t;
		System.out.printf("sumArraySeq| Result=%d in %d ms%n", sum, t);

		// Soma com mÃºltiplas threads - versÃ£o 1
		t = System.currentTimeMillis();
		sum = sumArrayMT1(numberOfThreads, array);
		t = System.currentTimeMillis() - t;
		System.out.printf("sumArrayMT1 | Result=%d in %d ms%n", sum, t);
	
		// Soma com mÃºltiplas threads - versÃ£o 2
		t = System.currentTimeMillis();
		sum = sumArrayMT2(numberOfThreads, array);
		t = System.currentTimeMillis() - t;
		System.out.printf("sumArrayMT2 | Result=%d in %d ms%n", sum, t);

		// Soma com mÃºltiplas threads - versÃ£o 3
		t = System.currentTimeMillis();
		sum = sumArrayMT3(numberOfThreads, array);
		t = System.currentTimeMillis() - t;
		System.out.printf("sumArrayMT3 | Result=%d in %d ms%n", sum, t);
	}

	static int[] buildArray(int size) {
		int[] array = new int[size];
		Random r = new Random(0);
		for (int i = 0; i < size; i++) {
		array[i] = -100 + r.nextInt(201); // -100 to 100
		}
		return array;
	}

	// Soma sequencial.
	static int sumArraySeq(int[] array) {
		int sum = 0;
		for (int i = 0; i < array.length; i++) {
		sum += array[i];
		}
		return sum;
	}

	static int sumArrayMT1 (int numberOfThreads, int[] array) throws InterruptedException {
		Thread[] t = new Thread[numberOfThreads];
		Result total = new Result();
		int blockSize = array.length / numberOfThreads;

		for (int i=0; i < t.length; i++) {
			int id = i;
			t[i] = new Thread(() -> {
				int localSum = 0; 
				int start = id * blockSize;
				int end = Math.min(array.length, start + blockSize);

				for (int j=start; j < end; j++)
					localSum += array[j];
				total.add(localSum);
			});
			t[i].start();
		} 

		for (int i=0; i < t.length; i++)
			t[i].join();
		return total.getValue();
	}

	static int sumArrayMT2(int numberOfThreads, int[] array) throws InterruptedException {
		Thread[] t = new Thread[numberOfThreads];
		AtomicInteger value = new AtomicInteger(0);

		for (int i=0; i < t.length; i++) {
			int id = i;
			t[i] = new Thread(() -> {
				int localSum = 0;
				int localSize = array.length / numberOfThreads;
				int start = id * localSize;
				int end = start + localSize;
				
				for (int j=start; j < end; j++) 
					localSum += array[j];
				value.addAndGet(localSum);
			});
			t[i].start();
		}

		for (int i=0; i < t.length; i++)
			t[i].join();
		return value.get();
	}

	static int sumArrayMT3(int numberOfThreads, int[] array) throws InterruptedException {
		Thread[] t = new Thread[numberOfThreads];
		int[] resultados_parciais = new int[numberOfThreads];

		for (int i=0; i < t.length; i++) {
			int id = i;
			t[i] = new Thread(() -> {
				int localSum = 0;
				int localSize = array.length / numberOfThreads;
				int start = id * localSize;
				int end = start + localSize;

				for (int j=start; j < end; j++) 
					localSum += array[j];
				resultados_parciais[id] = localSum;
			});
			t[i].start();
		}

		for (int i=0; i < t.length; i++)
			t[i].join();
		
		int sum=0;
		for (int i=0; i < resultados_parciais.length; i++) 
			sum += resultados_parciais[i];
		return sum;
	}
}

class Result {
	private int value;

	public synchronized void add(int v) { value += v; } 
	public synchronized int getValue() { return value; }
}
