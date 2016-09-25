char arr[8][2][] = 
{
	{"Blue", "0 0 255"},
	{"Red", "255 0 0"},
	{"Green", "0 255 0"},
	{"Orange", "255 170 0"},
	{"Yellow", "220 255 0"},
	{"Cyan", "0 255 255"},
	{"Pink", "255 0 255"},
	{"Purple", "125 0 255"},
}  

public int main() {
	return sizeof(arr[][]);
}
