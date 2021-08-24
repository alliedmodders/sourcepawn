
stock ProduceLittleEndian(any value, char[] output) {
	output[0] = ((value << 24) >> 24) & 0x000000FF;
	output[1] = ((value << 16) >> 24) & 0x000000FF;
	output[2] = ((value << 8) >> 24) & 0x000000FF;
	output[3] = (value >> 24) & 0x000000FF;
}

public main()
{
	char blah[4];
	ProduceLittleEndian(5, blah);
}
