function data_notched=simple_notch_filter(data,freqs,samp_freq)
%% 
% Inputs
%   * data = single vector (1 x n) of data to be notched (one channel at a time)
%   * freqs = vector of frequencies to be removed (in Hz)
%   * samp_freq = sampling frequency of the data (in Hz)
% Outputs
%   data_notched = filtered data
%
%elr 2021
%%

for j=1:length(freqs)
    notchFreq=freqs(j);
    fprintf(['Notch Filters: ' int2str(notchFreq) 'Hz.. \n'])
    [b,a]=fir2(samp_freq,[0 notchFreq-1.5 notchFreq-1 notchFreq+1 notchFreq+1.5 samp_freq/2]/(samp_freq/2),[1 1 0 0 1 1 ]);
	data=filtfilt(b,a,data')';
end
fprintf('\nNotch Filters Applied\n')
data_notched = data;
